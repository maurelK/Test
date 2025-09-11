from django.contrib import admin
from . models import (Event, Investor, Startup, Founder, News, NewsDetail, Partner, CustomUser)
# Register your models here.
admin.site.register(Event)
admin.site.register(Investor)
admin.site.register(Startup)
admin.site.register(Founder)
admin.site.register(News)
admin.site.register(NewsDetail)
admin.site.register(Partner)
admin.site.register(CustomUser)


