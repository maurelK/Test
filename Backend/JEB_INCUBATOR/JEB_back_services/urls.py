from django.urls import path
from . import views
from django.urls import path, include
from rest_framework import routers
from .views import EventViewSet
from .views import NewsViewSet
from .views import InvestorViewSet
from .views import StartupViewSet
from .views import FounderViewSet
from .views import PartnerViewSet
from .views import UserViewSet

router = routers.DefaultRouter()
router.register(r'events', EventViewSet)
router.register(r'news', NewsViewSet)
router.register(r'investors', InvestorViewSet)
router.register(r'startup', StartupViewSet)
router.register(r'founder', FounderViewSet)
router.register(r'partner', PartnerViewSet)
router.register(r'user', UserViewSet)

from .views import login_view
from django.urls import path

urlpatterns = [
    path('', include(router.urls)),
    path('login/', login_view, name='login'),
]
