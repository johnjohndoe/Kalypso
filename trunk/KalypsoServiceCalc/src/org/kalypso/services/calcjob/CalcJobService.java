package org.kalypso.services.calcjob;

import java.rmi.Remote;


/**
 * <p>Interface für CalcJobService</p>
 * 
 * <p>Der CalcJobService ist ein Dienst um Berechnungen auszuführen.</p>
 * <p>Das ganze funktioniert ähnlich wie eine Druckerwarteschlange. 
 * Ein Auftrag wird gesendet und in eine Liste gestellt, welche (normalerweise der Reihe nach) abgearbeitet wird.
 * Jeder Auftrag erhält dabei eine eindeutige ID. Ist eine Auftrag fertig kann das Ergebnis abgeholt werden.
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
	 * Gibt die IDs aller unterstützten JobTypen zurück.
	 *
	 * @throws CalcJobServiceException
	 */	
  public String[] getJobTypes() throws CalcJobServiceException;
  
  /**
   * Gibt die IDs aller vom Dienst bearbeiteten Aufträge zurück.
   * 
   * @throws CalcJobServiceException
   */
  public String[] getJobs() throws CalcJobServiceException;
	
	/**
	 * Erzeugt einen neuen Auftrag. Der Auftrag wird in die Liste gestellt und
	 * je nach Implementation abgearbeitet.
	 */  
  public String createJob( final String typeID, final String description, final String[] inputURLs ) throws CalcJobServiceException;
  
  /**
   * Gibt die BEschreibung des Auftrags zurück.
   * 
   */
  public CalcJobDescription getJobDescription( final String jobID ) throws CalcJobServiceException;

  /**
   * Stoppt einen Auftrag
   */
  public void cancelJob( final String jobID ) throws CalcJobServiceException;
  
  /**
   * <p>Löscht einen Auftrag vollständig<p>
   * <p>Löscht auch alle Dateien, die zu diesem Job gehörten, d.h. die URLs
   * von {@link #retrieveResults(String)} sind nicht mehr gültig</p>
   */
  public void removeJob( final String jobID ) throws CalcJobServiceException;
  
  /**
   * Gibt die Ergebnisse eines Auftrags zurück.
   * Falls der Auftrag noch nicht abgearbeitet wird, gibts ne Exception.
   * 
   * @return Die String-Repräsentation von URLs
   */
  public String[] retrieveResults( final String jobID ) throws CalcJobServiceException;
}

