from django.shortcuts import render
from django.http import HttpResponse
from .models import Event, News, Investor, Startup, Founder, Partner, CustomUser
from .serializers import (
    EventSerializer,
    NewsSerializer,
    InvestorSerializer,
    StartupSerializer,
    FounderSerializer,
    PartnerSerializer,
    UserSerializer,
)
from rest_framework import viewsets, status
from rest_framework.decorators import api_view
from rest_framework.response import Response


# Create your views here.

class EventViewSet(viewsets.ModelViewSet):
    queryset = Event.objects.all()
    serializer_class = EventSerializer


class NewsViewSet(viewsets.ModelViewSet):
    queryset = News.objects.all()
    serializer_class = NewsSerializer


class InvestorViewSet(viewsets.ModelViewSet):
    queryset = Investor.objects.all()
    serializer_class = InvestorSerializer


class StartupViewSet(viewsets.ModelViewSet):
    queryset = Startup.objects.all()
    serializer_class = StartupSerializer


class FounderViewSet(viewsets.ModelViewSet):
    queryset = Founder.objects.all()
    serializer_class = FounderSerializer


class PartnerViewSet(viewsets.ModelViewSet):
    queryset = Partner.objects.all()
    serializer_class = PartnerSerializer


class UserViewSet(viewsets.ModelViewSet):
    queryset = CustomUser.objects.all()
    serializer_class = UserSerializer


@api_view(['POST'])
def login_view(request):
    email = request.data.get('email')
    password = request.data.get('password')

    try:
        user = CustomUser.objects.get(email=email)
        if user.check_password(password):
            return Response(
                {'message': 'Login successful'},
                status=status.HTTP_200_OK
            )
        else:
            return Response(
                {'error': 'Email ou mot de passe erroné'},
                status=status.HTTP_401_UNAUTHORIZED
            )
    except CustomUser.DoesNotExist:
        return Response(
            {'error': 'Email ou mot de passe erroné'},
            status=status.HTTP_401_UNAUTHORIZED
        )
