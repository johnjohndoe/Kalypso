package org.kalypso.services.calculation;

import java.rmi.Remote;


/**
 * <p>Interface f�r ICalculationService</p>
 * 
 * <p>Der ICalculationService ist ein Dienst um Berechnungen auszuf�hren.</p>
 * <p>Das ganze funktioniert �hnlich wie eine Druckerwarteschlange. 
 * Ein Auftrag wird gesendet und in eine Liste gestellt, welche (normalerweise der Reihe nach) abgearbeitet wird.
 * Jeder Auftrag erh�lt dabei eine eindeutige ID. Ist eine Auftrag fertig kann das Ergebnis abgeholt werden.
 * 
 * @author belger
 */
public interface ICalculationService extends Remote 
{
	/**
	 * Gibt die IDs aller unterst�tzten JobTypen zur�ck.
	 *
	 * @throws CalcJobServiceException
	 */	
  public String[] getJobTypes() throws CalcJobServiceException;
  
  /**
   * Gibt den aktuellen Zustand aller vorhandenen {@link ICalcJob} zur�ck
   * 
   * @throws CalcJobServiceException
   */
  public CalcJobBean[] getJobs() throws CalcJobServiceException;
	
	/**
	 * Erzeugt einen neuen Auftrag. Der Auftrag wird in die Liste gestellt und
	 * je nach Implementation abgearbeitet.
	 */  
  public String createJob( final String typeID, final String description, final String[] inputURLs ) throws CalcJobServiceException;
  
  /**
   * Stoppt einen Auftrag
   */
  public void cancelJob( final String jobID ) throws CalcJobServiceException;
  
  /**
   * <p>L�scht einen Auftrag vollst�ndig<p>
   * <p>L�scht auch alle Dateien, die zu diesem Job geh�rten, d.h. die URLs
   * von {@link CalcJobBean#getResults()} sind nicht mehr g�ltig</p>
   * <p>Darf nur aufgerufen werden, wenn der Job gecanceled oder fertig ist</p> 
   */
  public void disposeJob( final String jobID ) throws CalcJobServiceException;
}

