package org.kalypso.services.calcjob;

import java.awt.Color;

/**
 * @author Belger
 */
public class CalcJobStatus
{
  public final static int UNKNOWN = -1;
  public final static int RUNNING = 0;
  public final static int FINISHED = 1;
  public final static int CANCELED = 2;
  public final static int WAITING = 3;
  public final static int ERROR = 4;
  
  public final static CalcJobStatus UNKNOWN_STATE = new CalcJobStatus( UNKNOWN, "UNKNOWN", Color.BLACK );
  public final static CalcJobStatus WAITING_STATE = new CalcJobStatus( WAITING, "WAITING", Color.CYAN );
  public final static CalcJobStatus RUNNING_STATE = new CalcJobStatus( RUNNING, "RUNNING", Color.YELLOW );
  public final static CalcJobStatus FINISHED_STATE = new CalcJobStatus( FINISHED, "FINISHED", Color.GREEN );
  public final static CalcJobStatus CANCELED_STATE = new CalcJobStatus( CANCELED, "CANCELED", Color.WHITE );
  public final static CalcJobStatus ERROR_STATE = new CalcJobStatus( ERROR, "ERROR", Color.RED );
  
  public static CalcJobStatus getJobState( final int id )
  {
    switch( id )
    {
      case UNKNOWN:
      return UNKNOWN_STATE;
      
      case RUNNING:
      return RUNNING_STATE;
      
      case FINISHED:
      return FINISHED_STATE;
      
      case CANCELED:
      return CANCELED_STATE;
      
      case ERROR:
      return ERROR_STATE;

      case WAITING:
      return WAITING_STATE;
      
      default:
      throw new IllegalArgumentException( "Ilegal CalcJobStatus id: " + id );
    }
  }

  public int id;
  public String name;
  public Color color;
  
  private CalcJobStatus( final int idParm, final String nameParm, final Color colorParm )
  {
    this.id = idParm;
    this.name = nameParm;
    this.color = colorParm;
  }
}
