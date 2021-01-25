from ..items import AmazonSpiderItem
import scrapy

class CrawlAmazonSpider(scrapy.Spider):
    name = "amazon"
    page_num = 2
    start_urls = [
        # Sci-Fi-Books
        # "https://www.amazon.in/s?i=stripbooks&bbn=976390031&rh=n%3A976389031%2Cn%3A1402038031%2Cp_n_publication_date%3A2684819031%2Cp_n_feature_three_browse-bin%3A9141482031&dc&qid=1611460927&rnid=976390031&ref=sr_nr_n_11"
        # Crime-Thriller-Books
        "https://www.amazon.in/s?i=stripbooks&bbn=976390031&rh=n%3A976389031%2Cn%3A1318161031%2Cp_n_publication_date%3A2684819031%2Cp_n_feature_three_browse-bin%3A9141482031&dc&qid=1611476080&rnid=976390031&ref=sr_nr_n_9"
    ]

    def parse(self, response):
        items = AmazonSpiderItem()

        all_books = response.css(".s-latency-cf-section")

        for book in all_books:
            book_title = book.css(".a-color-base.a-text-normal::text").extract()
            book_author = book.css(".a-color-secondary .a-size-base+ .a-size-base").css("::text").extract()
            book_price = book.css(".a-spacing-top-mini+ .a-spacing-top-mini .a-spacing-mini:nth-child(1) .a-link-normal.a-text-bold , .a-price-whole , .a-spacing-top-small .a-text-bold").css("::text").extract()
            book_imagelink = book.css(".s-image::attr(src)").extract()

            items["book_author"] = "".join([author.strip("\n") for author in book_author])
            items["book_price"] = " ".join([price.strip("\n") for price in book_price])

            if len(book_title) == 1:
                items["book_title"] = book_title[0]
            else:
                items["book_title"] = book_title

            try:
                items["book_imagelink"] = book_imagelink[0]
            except:
                items["book_imagelink"] = book_imagelink

            yield items

            # next_page = f"https://www.amazon.in/s?i=stripbooks&bbn=976390031&rh=n%3A976389031%2Cn%3A1402038031%2Cp_n_publication_date%3A2684819031%2Cp_n_feature_three_browse-bin%3A9141482031&dc&page={CrawlAmazonSpider.page_num}&qid=1611468424&rnid=976390031&ref=sr_pg_2"
            next_page = f"https://www.amazon.in/s?i=stripbooks&bbn=976390031&rh=n%3A976389031%2Cn%3A1318161031%2Cp_n_publication_date%3A2684819031%2Cp_n_feature_three_browse-bin%3A9141482031&dc&page={CrawlAmazonSpider.page_num}&qid=1611476083&rnid=976390031&ref=sr_pg_2"

            # alternate these variables for diff genre
            # scifi_num = 100
            crime_num = 75
            if CrawlAmazonSpider.page_num <= crime_num :
                yield response.follow(next_page, callback=self.parse)
                CrawlAmazonSpider.page_num += 1



