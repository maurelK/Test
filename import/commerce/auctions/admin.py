from django.contrib import admin
from .models import Listings,Bids,comments, Watchlist, User
# Register your models here.
admin.site.register(Listings)
admin.site.register(Bids)
admin.site.register(comments)
admin.site.register(Watchlist)
admin.site.register(User)