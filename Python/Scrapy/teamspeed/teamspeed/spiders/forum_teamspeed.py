from ..items import TeamspeedItem
from bs4 import BeautifulSoup as bs
import scrapy
import re

class ForumTeamspeedSpider(scrapy.Spider):
    name = "teamspeed"
    page_num=2
    start_urls = [
        ## gadgets and gaming
        "https://teamspeed.com/forums/gadgets-electronics-home-theater-gaming/?daysprune=-1"
    ]

    def parse(self, response):
        items = TeamspeedItem()

        soup = bs(response.text, "html.parser")
        all_posts = soup.find_all("div", {"class":"trow text-center"})

        for post in all_posts:
            header = post.select("h4 a")[0].getText()
            author = [tag.getText() for tag in post.find_all("a", {"rel":"nofollow"})][0]
            try:
                last_post = post.find("div", {"class":"smallfont text-right"}).getText()
                last_post =  " ".join(re.sub(r'[\t\r\n]', ' ', last_post).split())
            except:
                last_post = None
            try:
                replies = [tag.getText() for tag in post.find_all("a", {"rel":"nofollow"})][1]
            except:
                replies = None
            try:
                views =  post.select("div span")[-1].getText()
            except:
                views = None

            items["header"] = header
            items["author"] = author
            items["last_post"] = last_post
            items["replies"] = replies
            items["views"] = views

            yield items

        next_page = f"https://teamspeed.com/forums/gadgets-electronics-home-theater-gaming/index{ForumTeamspeedSpider.page_num}.html?daysprune=-1"
        if ForumTeamspeedSpider.page_num <=35:
            yield response.follow(next_page, callback=self.parse)
            ForumTeamspeedSpider.page_num +=1