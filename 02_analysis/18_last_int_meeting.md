SIVOCS \| Model building
================

## Principal Feature Analysis

> In some applications it might be desired to pick a subset of the
> original features rather then find a mapping that uses all of the
> original features. The benefits of finding this subset of features
> could be in saving cost of computing unnecessary features, saving cost
> of sensors (in physical measurement systems), and in excluding noisy
> features while keeping their information using “clean” features (for
> example tracking points on a face using easy to track points- and
> inferring the other points based on those few measurements).

How does the algorithm look like:

``` python:
class PFA(object):
    def __init__(self, n_features, q=None):
        self.q = q
        self.n_features = n_features

    def fit(self, X):
        if not self.q:
            self.q = X.shape[1]

        sc = StandardScaler()
        X = sc.fit_transform(X)

        pca = PCA(n_components=self.q).fit(X)
        A_q = pca.components_.T

        kmeans = KMeans(n_clusters=self.n_features).fit(A_q)
        clusters = kmeans.predict(A_q)
        cluster_centers = kmeans.cluster_centers_

        dists = defaultdict(list)
        for i, c in enumerate(clusters):
            dist = euclidean_distances([A_q[i, :]], [cluster_centers[c, :]])[0][0]
            dists[c].append((i, dist))

        self.indices_ = [sorted(f, key=lambda x: x[1])[0][0] for f in dists.values()]
        self.features_ = X[:, self.indices_]
```

## Results of PFA

-   PFA has been looped **1000** times, feature frequencies are as
    follows

<!-- -->

    ##  [1] "impulseForNonAcad.soc."        "impactTargetGroup.acad."      
    ##  [3] "groupsInvolved.res."           "concepts.data."               
    ##  [5] "groupsInvolved.media."         "natureOfInvolvement.welfare." 
    ##  [7] "groupsInvolved.civsoc."        "impactTargetGroup.busi."      
    ##  [9] "targetGroupsGoals.improve."    "benefitForNonAcademy"         
    ## [11] "concepts2"                     "groupsInvolved.citiz."        
    ## [13] "Impactstatements.unknown."     "kindOfChange.welfare."        
    ## [15] "kindOfChange.acad."            "impactTargetGroup.civsoc."    
    ## [17] "transdisciplinaryExp.rate."    "Impactstatements.mitig."      
    ## [19] "impactTargetGroup.pub."        "Impactstatements.emanc."      
    ## [21] "motivation.pheno."             "dissChannels.platf."          
    ## [23] "targetGroupsGoals.socgroups."  "impactTargetGroup.welfare."   
    ## [25] "Impactstatements.unaddressed." "dissChannels.prof."           
    ## [27] "natureOfInvolvement.media."    "impactTargetGroup.socgr."     
    ## [29] "dissChannels.mono."            "concepts.review."             
    ## [31] "adoptByPolicyHow.SQ002."       "dissChannels.peer."           
    ## [33] "dissChannels.policy."          "concepts.pub."                
    ## [35] "natureOfInvolvement.busi."     "dissChannels.web."            
    ## [37] "concepts3"                     "dissChannels.conf."           
    ## [39] "adoptByPolicy.rate."           "adoptByPolicyHow.SQ003."

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
