# Define your item pipelines here
#
# Don't forget to add your pipeline to the ITEM_PIPELINES setting
# See: https://docs.scrapy.org/en/latest/topics/item-pipeline.html


# useful for handling different item types with a single interface
from itemadapter import ItemAdapter

from pymongo import MongoClient
import sqlite3

class TeamspeedPipeline:
    def __init__(self):
        self.create_connection()
        self.create_table()

        self.client = MongoClient("mongodb://127.0.0.1:27017/")
        # self.collection = self.client["scrapy-db"].teamspeed_gadgets_and_gaming
        # self.collection = self.client["scrapy-db"].teamspeed_travel_hotels_and_food
        self.collection = self.client["scrapy-db"].teamspeed_photography_and_art

    def create_connection(self):
        # self.conn = sqlite3.connect("./data/teamspeed-gadgets-and-gaimng.db")
        # self.conn = sqlite3.connect("./data/teamspeed-travel-hotels-and-food.db")
        self.conn = sqlite3.connect("./data/teamspeed-photography_and_art.db")
        self.cur = self.conn.cursor()

    def create_table(self):
        self.cur.execute("""CREATE TABLE IF NOT EXISTS teamspeed_data(
                header text,
                author text,
                last_post text,
                replies text,
                views text            
        ) """)


    def process_item(self, item, spider):
        self.store_db(item)

    def store_db(self, item):
        a, b, c, d, e = item["header"], item["author"], item["last_post"], item["replies"], item["views"]

        self.cur.execute("""INSERT INTO teamspeed_data VALUES (?,?,?,?,?)""", (a, b, c, d, e))
        self.conn.commit()

        self.collection.insert({
            "header": a,
            "author": b,
            "last_post": c,
            "replies": d,
            "views":e
        })
