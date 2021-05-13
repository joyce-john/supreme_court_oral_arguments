# Why were these cases excluded from the analysis?

This analysis relies on information in the [Supreme Court Database](http://scdb.wustl.edu/data.php) maintained by the Washington University St. Louis to analyze the justices' voting patterns. Particularly, it looks at the **partyWinning** column to determine if the court sided with the petitioner or the respondent. These cases combine multiple dockets, and the parties may have the role of petitioner in one docket and respondent in another. This greatly complicates the automated process of parsing documents and analyzing the justices' voting patterns. Because this is a small portion of the data for the 2019 session, it was practical to simply exclude these cases from the analysis.

