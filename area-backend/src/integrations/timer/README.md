# Timer Service

Résumé
-------
Le `TimerService` est une intégration de type ACTION (trigger) pour le backend AREA. Il permet de décider si une AREA de type `timer_interval` doit se déclencher en comparant un intervalle (en minutes) avec la date de dernière exécution (`lastTriggered`).

Objectif
--------
- Fournir un trigger réutilisable `timer_interval`.
- Exposer une logique simple et testable : renvoyer `true` si l'intervalle est écoulé (ou si l'AREA n'a jamais été exécutée), `false` sinon.

Contrat (API interne)
---------------------
- Nom du service : `timer`
- Action / trigger : `timer_interval`
- Paramètres attendus :
  - `intervalMinutes` (number, requis) — intervalle en minutes
  - `lastTriggered` (string ISO, optionnel) — horodatage ISO de la dernière exécution
- Méthode : `checkAction(actionId: string, params: any, userToken?: string): Promise<boolean>`
  - Retour : `true` = déclencher l'AREA, `false` = ne pas déclencher

Comportement
-----------
- Si `intervalMinutes` est invalide (non-number ou ≤ 0) -> retourne `false`.
- Si `lastTriggered` absent -> retourne `true` (première exécution).
- Si `lastTriggered` invalide (non ISO parsable) -> retourne `true` (stratégie prudente).
- Sinon : compare `(now - lastTriggered) >= intervalMinutes` et retourne le résultat boolean.

Fichiers ajoutés / modifiés
--------------------------
- `src/integrations/timer/timer.service.ts` — implémentation du service.
- `src/integrations/timer/timer.module.ts` — module d'export du service.
- `src/integrations/timer/timer.service.spec.ts` — tests unitaires Jest.
- `test-scripts/check-timer-standalone.ts` — script de vérification rapide (ts-node).
- `src/debug/debug.controller.ts` and `src/debug/debug.module.ts` — endpoint debug `POST /api/debug/timer-check` (pour tests rapides HTTP).
- Modifications : `src/integrations/integrations.module.ts`, `src/integrations/service-registry.service.ts`, `src/app.module.ts` (enregistrement et exposition du service).

Comment tester localement
------------------------
Prérequis : node/npm, dépendances installées (`npm install` dans `area-backend/`).

1) Lancer le serveur (depuis le dossier `area-backend`)

```bash
npm run start:dev
```

2) Tester via l'endpoint debug HTTP (préfixe global `/api`)

Cas « jamais déclenché » (doit renvoyer `true`)

```bash
curl -s -X POST http://localhost:8080/api/debug/timer-check \
  -H 'Content-Type: application/json' \
  -d '{"intervalMinutes":5}' | jq .
# -> { "shouldTrigger": true }
```

Cas « lastTriggered il y a 2 minutes, interval 5 » (doit renvoyer `false`)

```bash
TS=$(date -u -d '2 minutes ago' --iso-8601=seconds)
curl -s -X POST http://localhost:8080/api/debug/timer-check \
  -H 'Content-Type: application/json' \
  -d "{\"intervalMinutes\":5,\"lastTriggered\":\"$TS\"}" | jq .
# -> { "shouldTrigger": false }
```

3) Tests unitaires (Jest)

```bash
cd area-backend
npm run test
# ou uniquement le test timer
npx jest src/integrations/timer/timer.service.spec.ts --verbose
```

4) Test rapide sans Nest

```bash
npx ts-node test-scripts/check-timer-standalone.ts
# Affiche: true, false, true
```

Message pour la PR (exemple)
---------------------------
Titre : feat(timer): add TimerService trigger

Corps (suggéré) :

```
Ajout du `TimerService` (trigger `timer_interval`) :
- Implémentation de la logique de trigger (checkAction)
- Module et enregistrement dans le `ServiceRegistry`
- Tests unitaires (Jest) et script de vérification rapide
- Endpoint debug `POST /api/debug/timer-check` pour tests manuels

Ce service permet de déclencher des AREAs basées sur un intervalle de temps.
Usage de démonstration et instructions de test incluses dans `src/integrations/timer/README.md`.
```

Checklist PR
-----------
- [ ] Code ajouté/édité est testé localement
- [ ] Tests unitaires passent
- [ ] README et/ou documentation ajoutée
- [ ] Modifications enregistrées dans la branche `feat/backend-areas-module`

Prochaines améliorations possibles
---------------------------------
- Scheduler/cron central qui itère sur les AREAs actives et déclenche automatiquement les timers (prototype possible).
- Persistance de `last_triggered` en base (Supabase) pour survivre aux redémarrages.
- Support d'autres paramètres (ex: timezone, jitter, maxExecutions).

Contact
-------
Pour toute question technique, demandez-moi ou mentionnez `@Maurel` dans la PR.
