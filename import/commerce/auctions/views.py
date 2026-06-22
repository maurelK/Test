import decimal

from django.contrib.auth import authenticate, login, logout
from django.db import IntegrityError
from django.http import HttpResponse, HttpResponseRedirect
from django.shortcuts import render, get_object_or_404, redirect
from django.urls import reverse
from .models import Listings, Bids, comments, Watchlist
from django.contrib.auth.decorators import login_required
from django.contrib import messages
from .forms import create_listingsForm
from django.shortcuts import render
from .models import Listings
from decimal import Decimal, InvalidOperation


from .models import User

@login_required

def index(request):
    return render(request, "auctions/index.html")


def login_view(request):
    if request.method == "POST":

        # Attempt to sign user in
        username = request.POST["username"]
        password = request.POST["password"]
        user = authenticate(request, username=username, password=password)

        # Check if authentication successful
        if user is not None:
            login(request, user)
            return HttpResponseRedirect(reverse("index"))
        else:
            return render(request, "auctions/login.html", {
                "message": "Invalid username and/or password."
            })
    else:
        return render(request, "auctions/login.html")

@login_required

def logout_view(request):
    logout(request)
    return HttpResponseRedirect(reverse("index"))


def register(request):
    if request.method == "POST":
        username = request.POST["username"]
        email = request.POST["email"]

        # Ensure password matches confirmation
        password = request.POST["password"]
        confirmation = request.POST["confirmation"]
        if password != confirmation:
            return render(request, "auctions/register.html", {
                "message": "Passwords must match."
            })

        # Attempt to create new user
        try:
            user = User.objects.create_user(username, email, password)
            user.save()
        except IntegrityError:
            return render(request, "auctions/register.html", {
                "message": "Username already taken."
            })
        login(request, user)
        return HttpResponseRedirect(reverse("index"))
    else:
        return render(request, "auctions/register.html")


def index(request):
    listings = Listings.objects.all()
    return render(request, "auctions/index.html", {"listings": listings})


def categorie(request):
    # Récupérer les catégories distinctes
    categories = Listings.objects.values_list('category', flat=True).distinct()

    # Vérifiez si des catégories existent
    if categories:
        return render(request, "auctions/categories.html", {"categories": categories})
    else:
        # Si aucune catégorie n'est trouvée, afficher un message ou une page vide
        return render(request, "auctions/categories.html", {"categories": []})

@login_required
def watchlist(request):
    watchlist_items = Watchlist.objects.all()
    return render(request, "auctions/Watchlist.html", {"watchlist": watchlist_items})

def create_listing(request):
    if request.method == 'POST':
        form = create_listingsForm(request.POST)
        if form.is_valid():
            title = form.cleaned_data["title"]
            description = form.cleaned_data["description"]
            start_price = form.cleaned_data["start_price"]
            image_url = form.cleaned_data["image_url"]
            category = form.cleaned_data["category"]


            form.save()  # Save the form data to the database
            return render(request, "auctions/index.html")
    else:
        form = create_listingsForm()
        return render(request, "auctions/create_list.html", {'form': form})

@login_required
def listing_page(request, listing_id):
    listing = get_object_or_404(Listings, id=listing_id)
    
    user_watchlist = listing.watchlist_set.filter(user=request.user) if request.user.is_authenticated else None
    highest_bid = Bids.objects.filter(aution_listings=listing).order_by('-amount').first()
    if request.method == "POST":
        if "action" in request.POST:
            if user_watchlist.exists():
                user_watchlist.delete()
                messages.success(request, "Removed from watchlist.")
            else:
                Watchlist.objects.create(user=request.user, listing=listing)
                messages.success(request, "Added to watchlist.")
        elif "place_bid" in request.POST:
            print(request.POST.get("bid_amount"))

            bid_amount = decimal.Decimal(request.POST["bid_amount"])
            if bid_amount < listing.start_price or (highest_bid and bid_amount <= highest_bid.amount):
                messages.error(request, "Invalid bid amount.")
            else:
                Bids.objects.create(user=request.user, aution_listings=listing, amount=bid_amount)
                listing.save()
                messages.success(request, "Bid placed successfully.")
        elif "close_auction" in request.POST and listing.created_by == request.user:
            listing.active = False

            listing.save()
            messages.success(request, "Auction closed.")

        elif "add_comment" in request.POST:
            content = request.POST["comment"]
            comments.objects.create(user=request.user, aution_listings=listing, comment=content)
            messages.success(request, "Comment added.")
        
        return redirect("listing_page", listing_id=listing.id)

    return render(request, "auctions/listing_page.html", {
        "listing": listing,
        "highest_bid": highest_bid,
        "is_creator": request.user == listing.created_by,
        "user_watchlist": user_watchlist,
        "comments": listing.comments.all(),
        "is_winner": highest_bid and highest_bid.user == request.user,
    })

