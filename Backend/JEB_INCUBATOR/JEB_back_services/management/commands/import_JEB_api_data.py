from django.core.management.base import BaseCommand
import requests
from JEB_back_services.models import (Event, Investor, Startup, Founder, News, NewsDetail, Partner, CustomUser)
from dotenv import load_dotenv
from datetime import datetime
import os

load_dotenv()
API_TOKEN = os.getenv("API_TOKEN")

class Command(BaseCommand):
    help = 'Import data from JEB Incubator API'
    base_url = "https://api.jeb-incubator.com/"  # ← Corrigé

    def add_arguments(self, parser):  # ← Ajouté
        parser.add_argument(
            '--entity', 
            type=str, 
            default='all', 
            choices=['startups', 'investors', 'events', 'news', 'users', 'all'],
            help='Entity to import'
        )

    def get_headers(self):  # ← Renommé
        return {"X-Group-Authorization": API_TOKEN}

    def import_startups(self):
        """Import des startups"""
        self.stdout.write('Import des startups...')
        
        url = f'{self.base_url}/startups'
        response = requests.get(url, headers=self.get_headers())
        
        if response.status_code == 200:
            startups_data = response.json()
            created_count = 0
            updated_count = 0
            
            for startup_data in startups_data:        

                # Récupérer les détails si nécessaire
                if 'id' in startup_data:
                    detail_url = f'{self.base_url}/startups/{startup_data["id"]}'
                    detail_response = requests.get(detail_url, headers=self.get_headers())
                    
                    if detail_response.status_code == 200:
                        detailed_data = detail_response.json()
                    else:
                        detailed_data = startup_data
                else:
                    detailed_data = startup_data
                
                ###############################################"# Conversion de la date si présente
                #created_at = None
                #if detailed_data.get('created_at'):
                #    try:
                #        created_at = datetime.fromisoformat(
                #            detailed_data['created_at'].replace('Z', '+00:00')
                #        )
                #    except (ValueError, TypeError):
                #        created_at = None
                

                # Créer ou mettre à jour la startup
                startup, created = Startup.objects.update_or_create(
                    email=detailed_data.get('email', ''),
                    defaults={
                        'name': detailed_data.get('name', ''),
                        'legal_status': detailed_data.get('legal_status'),
                        'address': detailed_data.get('address'),
                        'phone': detailed_data.get('phone'),
                        'created_at': detailed_data.get('created_at'),
                        'description': detailed_data.get('description'),
                        'website_url': detailed_data.get('website_url'),
                        'social_media_url': detailed_data.get('social_media_url'),
                        'project_status': detailed_data.get('project_status'),
                        'needs': detailed_data.get('needs'),
                        'sector': detailed_data.get('sector'),
                        'maturity': detailed_data.get('maturity'),
                    }
                )
                
                if created:
                    created_count += 1
                    self.stdout.write(f'Créé: {startup.name}')
                else:
                    updated_count += 1
                    self.stdout.write(f'Mis à jour: {startup.name}')
                
                # Import des fondateurs si présents
                if 'founders' in detailed_data and detailed_data['founders']:
                    self._import_founders(startup, detailed_data['founders'])
            
            self.stdout.write(
                self.style.SUCCESS(
                    f'Startups: {created_count} créées, {updated_count} mises à jour'
                )
            )
        else:
            self.stdout.write(
                self.style.ERROR(f'Erreur import startups: {response.status_code}')
            )

    def _import_founders(self, startup, founders_data):
        """Import des fondateurs pour une startup"""
        # Supprimer les anciens fondateurs
        startup.founders.all().delete()
        
        # Créer les nouveaux
        for founder_data in founders_data:
            Founder.objects.create(
                startup=startup,
                name=founder_data.get('name', '')
            )

    def import_users(self):
        """Import des utilisateurs existants"""
        self.stdout.write('Import des utilisateurs...')
        
        url = f'{self.base_url}/users'
        response = requests.get(url, headers=self.get_headers())
        
        if response.status_code == 200:
            users_data = response.json()
            created_count = 0
            
            for user_data in users_data:
                # Vérifier si l'utilisateur existe déjà
                existing_user = CustomUser.objects.filter(
                    email=user_data.get('email', '')
                ).first()
                
                if not existing_user:
                    # Créer un nouvel utilisateur (il devra définir son mot de passe)
                    user = CustomUser.objects.create_user(
                        username=user_data.get('email', ''),
                        email=user_data.get('email', ''),
                        name=user_data.get('name', ''),
                        role=user_data.get('role', 'user'),
                        is_active=False  # Inactif tant qu'il n'a pas défini son mot de passe
                    )
                    
                    # Lier avec founder ou investor si IDs fournis
                    if user_data.get('founder_id'):
                        try:
                            founder = Founder.objects.get(id=user_data['founder_id'])
                            user.founder = founder
                            user.save()
                        except Founder.DoesNotExist:
                            pass
                    
                    if user_data.get('investor_id'):
                        try:
                            investor = Investor.objects.get(id=user_data['investor_id'])
                            user.investor = investor
                            user.save()
                        except Investor.DoesNotExist:
                            pass
                    
                    created_count += 1
                    self.stdout.write(f'Utilisateur créé: {user.email}')
                else:
                    self.stdout.write(f'Utilisateur existe déjà: {existing_user.email}')
            
            self.stdout.write(
                self.style.SUCCESS(f'{created_count} nouveaux utilisateurs créés')
            )
        else:
            self.stdout.write(
                self.style.ERROR(f'Erreur import utilisateurs: {response.status_code}')
            )

    def import_investors(self):
        """Import des investisseurs"""
        self.stdout.write('Import des investisseurs...')
        
        url = f'{self.base_url}/investors'
        response = requests.get(url, headers=self.get_headers())
        
        if response.status_code == 200:
            investors_data = response.json()
            count = 0
            
            for investor_data in investors_data:
                created_at = None
                if investor_data.get('created_at'):
                    try:
                        created_at = datetime.fromisoformat(
                            investor_data['created_at'].replace('Z', '+00:00')
                        )
                    except (ValueError, TypeError):
                        created_at = None
                
                investor, created = Investor.objects.update_or_create(
                    email=investor_data.get('email', ''),
                    defaults={
                        'name': investor_data.get('name', ''),
                        'legal_status': investor_data.get('legal_status'),
                        'address': investor_data.get('address'),
                        'phone': investor_data.get('phone'),
                        'created_at': created_at,
                        'description': investor_data.get('description'),
                        'investor_type': investor_data.get('investor_type'),
                        'investment_focus': investor_data.get('investment_focus'),
                    }
                )
                
                if created:
                    count += 1
            
            self.stdout.write(
                self.style.SUCCESS(f'{count} investisseurs importés')
            )
        else:
            self.stdout.write(
                self.style.ERROR(f'Erreur import investisseurs: {response.status_code}')
            )

    def import_events(self):
        """Import des événements"""
        self.stdout.write('Import des événements...')
        
        url = f'{self.base_url}/events'
        response = requests.get(url, headers=self.get_headers())
        
        if response.status_code == 200:
            events_data = response.json()
            count = 0
            
            for event_data in events_data:
                event, created = Event.objects.update_or_create(
                    name=event_data.get('name', ''),
                    defaults={
                        'dates': event_data.get('dates'),
                        'location': event_data.get('location'),
                        'description': event_data.get('description'),
                        'event_type': event_data.get('event_type'),
                        'target_audience': event_data.get('target_audience'),
                    }
                )
                
                if created:
                    count += 1
            
            self.stdout.write(
                self.style.SUCCESS(f'{count} événements importés')
            )
        else:
            self.stdout.write(
                self.style.ERROR(f'Erreur import événements: {response.status_code}')
            )

    def import_news(self):
        """Import des actualités"""
        self.stdout.write('Import des actualités...')
        
        url = f'{self.base_url}/news'
        response = requests.get(url, headers=self.get_headers())
        
        if response.status_code == 200:
            news_data = response.json()
            count = 0
            
            for news_item in news_data:
                # Conversion de la date
                news_date = None
                if news_item.get('news_date'):
                    try:
                        news_date = datetime.fromisoformat(
                            news_item['news_date'].replace('Z', '+00:00')
                        ).date()
                    except (ValueError, TypeError):
                        news_date = None
                
                # Lien avec startup si ID fourni
                startup = None
                if news_item.get('startup_id'):
                    try:
                        startup = Startup.objects.get(id=news_item['startup_id'])
                    except Startup.DoesNotExist:
                        pass
                
                news, created = News.objects.update_or_create(
                    title=news_item.get('title', ''),
                    news_date=news_date,
                    defaults={
                        'category': news_item.get('category'),
                        'location': news_item.get('location'),
                        'startup': startup,
                    }
                )
                
                if created:
                    count += 1
            
            self.stdout.write(
                self.style.SUCCESS(f'{count} actualités importées')
            )
        else:
            self.stdout.write(
                self.style.ERROR(f'Erreur import actualités: {response.status_code}')
            )

    def handle(self, *args, **options):
            entity = options.get('entity')
            
            if not API_TOKEN:
                self.stdout.write(
                    self.style.ERROR(
                        'JEB_GROUP_TOKEN non configuré dans settings.py'
                    )
                )
                return
            
            self.stdout.write(f'Début import depuis JEB Incubator API...')
            
            # Import selon l'entité demandée
            if entity == 'all':
                self.import_users()      
                self.import_investors()
                self.import_events()
                self.import_news()
            elif entity == 'startups':
                self.import_startups()
            elif entity == 'investors':
                self.import_investors()
            elif entity == 'events':
                self.import_events()
            elif entity == 'news':
                self.import_news()
            elif entity == 'users':
                self.import_users()
            
            self.stdout.write(
                self.style.SUCCESS('Import terminé!')
            )
    
    