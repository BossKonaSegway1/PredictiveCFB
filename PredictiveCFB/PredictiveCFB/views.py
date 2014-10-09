#!/usr/bin/env python
# -*- coding: utf-8 -*-

from django.shortcuts import render
from django.http import Http404
from django.http import HttpResponseRedirect, HttpResponse
from django.views.generic import TemplateView

class HomeView(TemplateView):
    """Home Page for the website"""
    
    template_name = 'PredictiveCFB/home.html'
    def get_context_data(self, **kwargs):
        context = super(HomeView, self).get_context_data(**kwargs)
        context['Authors'] = ["Sean C", "Rohit V", "Batts"]
        return context

