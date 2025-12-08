import { Matches } from "class-validator";
export class TimerTimeMatchDto
{
    @Matches(/^([01][09]|2[0-3]:[05][09])$/)
    //on fait aussi @Matches(/^([01]\d|2[0-3]:[05]\d)$/)
    //pour specifier qu'ils veulent des digites en mode 0 - 9 quoi les gars
    time:string;
}
