package org.kalypso.services.calcjob;

import java.net.URL;
import java.rmi.Remote;


/**
 * <p>Interface f�r CalcJobService</p>
 * 
 * <p>Der CalcJobService ist ein Dienst um Berechnungen auszuf�hren.</p>
 * <p>Das ganze funktioniert �hnlich wie eine Druckerwarteschlange. 
 * Ein Auftrag wird gesendet und in eine Liste gestellt, welche (normalerweise der Reihe nach) abgearbeitet wird.
 * Jeder Auftrag erh�lt dabei eine eindeutige ID. Ist eine Auftrag fertig kann das Ergebnis abgeholt werden.
 * 
 * @serviceInterface 
 *        name="CalcJob"
 *        description="Rechendienst"
 *        impl="threaded.CalcJobService_impl_Queued"
 *
 * @TODO: statt killJob cancelJob + removeJob
 *
 * @author belger
 */
public interface CalcJobService extends Remote 
{
	/**
	 * Gibt die IDs aller unterst�tzten JobTypen zur�ck.
	 *
	 * @throws CalcJobServiceException
	 */	
  public String[] getJobTypes() throws CalcJobServiceException;
  
  /**
   * Gibt die IDs aller vom Dienst bearbeiteten Auftr�ge zur�ck.
   * 
   * @throws CalcJobServiceException
   */
  public String[] getJobs() throws CalcJobServiceException;
	
	/**
	 * Erzeugt einen neuen Auftrag. Der Auftrag wird in die Liste gestellt und
	 * je nach Implementation abgearbeitet.
	 */  
  public String createJob( final String typeID, final String description, final URL[] inputURLs ) throws CalcJobServiceException;
  
  /**
   * Gibt die BEschreibung des Auftrags zur�ck.
   * 
   */
  public CalcJobDescription getJobDescription( final String jobID ) throws CalcJobServiceException;

  /**
   * Stoppt einen Auftrag
   */
  public void cancelJob( final String jobID ) throws CalcJobServiceException;
  
  /**
   * <p>L�scht einen Auftrag vollst�ndig<p>
   * <p>L�scht auch alle Dateien, die zu diesem Job geh�rten, d.h. die URLs
   * von {@link #retrieveResults(String)} sind nciht mehr g�ltig</p>
   */
  public void removeJob( final String jobID ) throws CalcJobServiceException;
  
  /**
   * Gibt die Ergebnisse eines Auftrags zur�ck.
   * Falls der Auftrag noch nicht abgearbeitet wird, gibts ne Exception.
   */
  public URL[] retrieveResults( final String jobID ) throws CalcJobServiceException;
}

