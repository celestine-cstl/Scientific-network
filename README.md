# Analyse de Réseau - Évolution d’un réseau scientifique

**Étude avant/après “traitement” : construction de graphes, mesures structurelles, distribution des degrés (log–log), attachement préférentiel, analyse par attributs (genre & statut core) et détection de communautés.** 

![Python](https://img.shields.io/badge/Python-3776AB?style=for-the-badge\&logo=python\&logoColor=white)
![Jupyter](https://img.shields.io/badge/Jupyter-F37626?style=for-the-badge\&logo=jupyter\&logoColor=white)
![Pandas](https://img.shields.io/badge/Pandas-150458?style=for-the-badge\&logo=pandas\&logoColor=white)
![NetworkX](https://img.shields.io/badge/NetworkX-333333?style=for-the-badge\&logo=networkx\&logoColor=white)
![Matplotlib](https://img.shields.io/badge/Matplotlib-11557C?style=for-the-badge\&logo=matplotlib\&logoColor=white)
![powerlaw](https://img.shields.io/badge/powerlaw-FF6F00?style=for-the-badge\&logo=python\&logoColor=white)
![Word](https://img.shields.io/badge/Word-2B579A?style=for-the-badge\&logo=microsoft-word\&logoColor=white)
![PowerPoint](https://img.shields.io/badge/PowerPoint-B7472A?style=for-the-badge\&logo=microsoft-powerpoint\&logoColor=white)


> Travail réalisé en **Python** : **Pandas/NumPy** pour l’ingestion, le nettoyage et les jointures nœuds–liens, **NetworkX** pour la construction et l’analyse des graphes non orientés, **Matplotlib/Seaborn** pour les visualisations, et **powerlaw** pour tester la loi de puissance sur la queue des degrés, **Word** a servi au rendu écrit et **PowerPoint** à la présentation orale duprojet.


<br>

## 🛠️ Compétences mobilisées

- **Network science** : densité, degré moyen, composantes connexes, composante géante, clustering (moyen/global), longueurs de plus courts chemins. 
- **Degree distribution** : histogrammes, échelle log–log, test d’hypothèse loi de puissance et estimation d'alpha 
- **Preferential attachment** : mesure de la préférence relative et cumulée sur les nœuds activés (rich-get-richer).
- **Attributs & homophilie** : assortativité, statistiques par sous-groupes (degré, isolement, centralités). 
- **Community detection** : Louvain, comparaison du nombre/taille des communautés avant/après. 

<br>

## 📌 Résultats clés

Après traitement, **le réseau apparaît plus connecté et moins fragmenté** : les collaborations se densifient, la part de nœuds isolés recule et la structure se recentre autour d’un noyau plus cohésif. La distribution des degrés devient plus hétérogène, avec une queue plus marquée indiquant l’**émergence de nœuds très connectés** ; l’analyse en log–log et l’ajustement en queue soutiennent ce diagnostic. La dynamique d’attachement préférentiel est renforcée : les nouveaux liens ont davantage tendance à viser les nœuds déjà bien connectés. Sur les attributs, les indicateurs suggèrent une **amélioration de l’inclusion selon le genre**, avec une réduction de l’isolement et un accès accru aux collaborations, tandis que le groupe non-core rattrape une partie de son retard en termes de connectivité et de positionnement dans le réseau. Enfin, la détection de communautés met en évidence des ensembles plus structurés et de plus grande taille, avec une composition interne plus variée, signe d’une **circulation accrue de l’information et d’une meilleure intégration globale**.
