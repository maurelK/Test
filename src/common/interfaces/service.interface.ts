// Structure d'une Action ou Réaction visible par le front
export interface ServiceActionDefinition {
  id: string; // ex: 'discord_send_message'
  name: string; // ex: 'Envoyer un message Discord'
  description: string;
  type: 'ACTION' | 'REACTION';
  parameters: {
    // Les champs que le front doit afficher
    name: string;
    type: 'string' | 'number' | 'boolean';
    required: boolean;
    description?: string;
  }[];
}

// Le contrat que chaque service (Discord, Spotify...) doit remplir
export interface IService {
  // Le nom unique du service (ex: 'discord')
  name: string;

  // Pour le about.json : liste ce que le service sait faire
  getDefinitions(): ServiceActionDefinition[];

  // Si c'est une ACTION (Trigger), cette méthode est appelée par le Cron Job
  // pour vérifier si la condition est remplie.
  checkAction?(
    actionId: string,
    params: any,
    userToken?: string,
  ): Promise<boolean>;

  // Si c'est une REACTION, cette méthode est appelée pour exécuter la tâche.
  executeReaction?(
    reactionId: string,
    params: any,
    userToken?: string,
  ): Promise<void>;
}
