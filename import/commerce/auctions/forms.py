from django import forms
from .models import Listings

class create_listingsForm(forms.ModelForm):
    CHOICES = [
        ('Option 1', 'Peinture'),
        ('Option 2', 'Fashion'),
        ('Option 3', 'Electroménager'),
        ('Option 4', 'Véhicules'),
        ('Option 5', 'Produits Informatique'),
        ('Option 6', 'Autres'),
    ]

    category = forms.ChoiceField(choices=CHOICES, required=False)

    class Meta:
        model = Listings
        fields = ["title", "description", "start_price", "image_url", "category"]
     

