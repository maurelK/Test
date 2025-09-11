from django.db import models
import datetime
from django.contrib.auth.models import AbstractUser

# Create your models here.


##RAPPEL SI DEMAIN ÇA N'INTEGRE PAS C'EST SUR QUE C'EST LE DATEFIELD OF CREATED_AT:.::::

class Event(models.Model):
    name = models.CharField(max_length=150)
    dates = models.CharField(max_length=250, null=True, blank=True)
    location = models.CharField(max_length=150, null=True, blank=True)
    description = models.CharField(max_length=450, null=True, blank=True)
    event_type = models.CharField(max_length=150, null=True, blank=True)
    target_audience = models.CharField(max_length=150, null=True, blank=True)

    def __str__(self):
        return self.name


class Investor(models.Model):
    name = models.CharField(max_length=255)
    legal_status = models.CharField(max_length=255, null=True, blank=True)
    address = models.CharField(max_length=255, null=True, blank=True)
    email = models.EmailField(max_length=255)
    phone = models.CharField(max_length=50, null=True, blank=True)
    created_at = models.DateTimeField(null=True, blank=True)
    description = models.TextField(null=True, blank=True)
    investor_type = models.CharField(max_length=255, null=True, blank=True)
    investment_focus = models.CharField(max_length=255, null=True, blank=True)

    def __str__(self):
        return self.name


class Startup(models.Model):
    name = models.CharField(max_length=255)
    legal_status = models.CharField(max_length=255, null=True, blank=True)
    address = models.CharField(max_length=255, null=True, blank=True)
    email = models.EmailField(max_length=255)
    phone = models.CharField(max_length=50, null=True, blank=True)
    created_at = models.DateTimeField(null=True, blank=True)
    description = models.TextField(null=True, blank=True)
    website_url = models.URLField(max_length=255, null=True, blank=True)
    social_media_url = models.JSONField(null=True, blank=True)
    project_status = models.CharField(max_length=100, null=True, blank=True)
    needs = models.CharField(max_length=255, null=True, blank=True)
    sector = models.CharField(max_length=100, null=True, blank=True)
    maturity = models.CharField(max_length=100, null=True, blank=True)

    def __str__(self):
        return self.name


class Founder(models.Model):
    startup = models.ForeignKey(Startup, on_delete=models.CASCADE, related_name="founders")
    name = models.CharField(max_length=150)

    def __str__(self):
        return f"{self.name} - {self.startup.name}"


class News(models.Model):
    startup = models.ForeignKey(Startup, on_delete=models.SET_NULL, null=True, blank=True)
    news_date = models.DateField(null=True, blank=True)
    location = models.CharField(max_length=150, null=True, blank=True)
    title = models.CharField(max_length=150, null=True, blank=True)
    category = models.CharField(max_length=150, null=True, blank=True)

    def __str__(self):
        return self.title or "News sans titre" # les methodes dite __str__ pour que dans l'adm django tu ne voye pas news.object1

    class Meta:
        verbose_name_plural = "News" #sinon come is il y a s dans News ça mettra Newss dans l'admin


class NewsDetail(models.Model):
    news_date = models.DateField(null=True, blank=True)
    location = models.CharField(max_length=150, null=True, blank=True)
    title = models.CharField(max_length=150, null=True, blank=True)
    category = models.CharField(max_length=150, null=True, blank=True)
    description = models.TextField(null=True, blank=True)
    startup = models.ForeignKey(Startup, on_delete=models.SET_NULL, null=True, blank=True)

    def __str__(self):
        return self.title or "NewsDetail sans titre"

    class Meta:
        verbose_name_plural = "News Details"


class Partner(models.Model):
    name = models.CharField(max_length=255)
    legal_status = models.CharField(max_length=255, null=True, blank=True)
    address = models.CharField(max_length=255, null=True, blank=True)
    email = models.EmailField(max_length=255)
    phone = models.CharField(max_length=50, null=True, blank=True)
    created_at = models.DateTimeField(null=True, blank=True)
    description = models.TextField(null=True, blank=True)
    partnership_type = models.CharField(max_length=255, null=True, blank=True)

    def __str__(self):
        return self.name


class CustomUser(AbstractUser):
    email = models.EmailField(unique=True)
    name = models.CharField(max_length=100)
    role = models.CharField(max_length=50)
    founder = models.ForeignKey(Founder, on_delete=models.SET_NULL, null=True, blank=True)
    investor = models.ForeignKey(Investor, on_delete=models.SET_NULL, null=True, blank=True)

    def __str__(self):
        return self.name or self.username