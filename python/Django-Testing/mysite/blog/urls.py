from django.urls import path
from . import views

urlpatterns = [
    path("",views.home, name="bog-home" ),
    path("about/",views.about, name="bog-about" ),

]