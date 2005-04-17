/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.services.calculation.service;

import java.rmi.Remote;

import org.kalypso.services.IKalypsoService;
import org.kalypso.services.calculation.job.ICalcJob;

/**
 * <p>
 * Interface f�r ICalculationService
 * </p>
 * 
 * <p>
 * Der ICalculationService ist ein Dienst um Berechnungen auszuf�hren.
 * </p>
 * <p>
 * Das ganze funktioniert �hnlich wie eine Druckerwarteschlange. Ein Auftrag
 * wird gesendet und in eine Liste gestellt, welche (normalerweise der Reihe
 * nach) abgearbeitet wird. Jeder Auftrag erh�lt dabei eine eindeutige ID. Ist
 * eine Auftrag fertig kann das Ergebnis abgeholt werden.
 * 
 * @author belger
 */
public interface ICalculationService extends Remote, IKalypsoService
{
  //  
  /**
   * Gibt die IDs aller unterst�tzten JobTypen zur�ck.
   * 
   * @throws CalcJobServiceException
   */
  public String[] getJobTypes() throws CalcJobServiceException;

  /**
   * Gibt den aktuellen Zustand aller vorhandenen {@link ICalcJob}zur�ck
   * 
   * @throws CalcJobServiceException
   */
  public CalcJobBean[] getJobs() throws CalcJobServiceException;

  /** Gibt den aktuellen Zustand eines einzelnen {@link ICalcJob}zur�ck */
  public CalcJobBean getJob( final String jobID ) throws CalcJobServiceException;

  /**
   * Erzeugt einen neuen Auftrag. Der Auftrag wartet erstmal auf die Daten.
   * 
   * Erzeugt insbsondere das Basis-Verzeichnis f�r die Eingangsdaten, in welches
   * diese vom Server kopiert werden m�ssen.
   */
  public CalcJobBean prepareJob( final String typeID, final String description )
      throws CalcJobServiceException;

  /**
   * Teilt dem Job mit, dass jetzt alle Daten dem Server �bergeben wurden. Der
   * Auftrag wird jetzt in die Liste gestellt und (abh�ngig von der
   * Imlpementation) baldm�glichst abgearbeitet.
   * 
   * @param input
   *          Die Eingabedateien, die der Client zur Verf�gung stellt.
   */
  public void startJob( final String jobID, final CalcJobDataBean[] input )
      throws CalcJobServiceException;

  /**
   * Stoppt einen Auftrag
   */
  public void cancelJob( final String jobID ) throws CalcJobServiceException;

  /**
   * <p>
   * L�scht einen Auftrag vollst�ndig
   * <p>
   * <p>
   * L�scht auch alle Dateien, die zu diesem Job geh�rten, d.h. die URLs von
   * {@link CalcJobBean#getResults()}sind nicht mehr g�ltig
   * </p>
   * <p>
   * Darf nur aufgerufen werden, wenn der Job gecanceled oder fertig ist
   * </p>
   */
  public void disposeJob( final String jobID ) throws CalcJobServiceException;
}
