from django.contrib.auth.models import AbstractUser
from django.db import models


class User(AbstractUser):
    pass

class Listings(models.Model):
    title = models.CharField(max_length=64)
    description = models.TextField()
    start_price = models.DecimalField(max_digits=10, decimal_places=2, null=False, blank=False)
    highest_bid = models.DecimalField(max_digits=10, decimal_places=2, null=False, blank=False, default="0.00")

    image_url = models.URLField(blank=True, null=True)
    active = models.BooleanField(default=True)
    category = models.CharField(max_length=50, null=True, blank=True)
    created_at = models.DateTimeField(auto_now_add=True)
    created_by = models.ForeignKey(
        User,
        on_delete=models.CASCADE,
        related_name="listings",
        default=1  
)


def __str__(self):
    return self.title

class Bids(models.Model):
    aution_listings = models.ForeignKey(Listings, on_delete=models.CASCADE, related_name='bids')
    user = models.ForeignKey(User, on_delete=models.CASCADE)
    amount = models.DecimalField(max_digits=10, decimal_places=2)
    created_at = models.DateTimeField(auto_now_add=True)

def __str__(self):
    return f"{self.user.username} bid is {self.amount} on {self.aution_listings} at {self.created_at}"

class comments(models.Model):
    aution_listings = models.ForeignKey(Listings, on_delete=models.CASCADE, related_name='comments')
    user = models.ForeignKey(User, on_delete=models.CASCADE)
    comment = models.TextField()
    created_at = models.DateTimeField(auto_now_add=True)

def __str__(self):
    return f"{self.user.username} commented {self.comment} on {self.aution_listings} at {self.created_at}"


class Watchlist(models.Model):
    user = models.ForeignKey(User, on_delete=models.CASCADE, related_name="watchlist")
    listing = models.ForeignKey(Listings, on_delete=models.CASCADE, default=1)

def __str__(self):
    return f"{self.user.username} is watching {self.listing}"