
import requests
from django.core.management.base import BaseCommand
from auctions.models import (Event, Investor, Startup, Founder, News, NewsDetail, Partner, CustomUser)
from dotenv import load_dotenv
import os
from datetime import datetime

load_dotenv()
API_TOKEN = os.getenv("API_TOKEN")

class Command(BaseCommand):
    help = 'Import data from JEB Incubator API'

    base_url = "https://api.jeb-incubator.com"

    def get_headers(self):
        return {"X-Group-Authorization": API_TOKEN}

    def handle(self, *args, **kwargs):
        self.import_startups()
        self.import_users()
        self.import_investors()
        self.import_events()
        self.import_news()

    # Ajoute ici les méthodes import_startups, import_users, import_investors, import_events, import_news
