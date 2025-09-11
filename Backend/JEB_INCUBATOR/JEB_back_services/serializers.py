from rest_framework import serializers
from .models import Event
from .models import News
from .models import Investor
from .models import Startup
from .models import Founder
from .models import Partner
from .models import CustomUser

class EventSerializer(serializers.ModelSerializer):
    class Meta:
        model = Event
        fields = "__all__"

class NewsSerializer(serializers.ModelSerializer):
    class Meta:
        model = News
        fields = "__all__"

class InvestorSerializer(serializers.ModelSerializer):
    class Meta:
        model = Investor
        fields = "__all__"

class StartupSerializer(serializers.ModelSerializer):
    class Meta:
        model = Startup
        fields = "__all__"

class FounderSerializer(serializers.ModelSerializer):
    class Meta:
        model = Founder
        fields = "__all__"

class PartnerSerializer(serializers.ModelSerializer):
    class Meta:
        model = Founder
        fields = "__all__"

class UserSerializer(serializers.ModelSerializer):
    password = serializers.CharField(write_only=True)

    class Meta:
        model = CustomUser
        fields = "__all__"

    def create(self, validated_data):
        user = CustomUser.objects.create_user(**validated_data)
        return user
