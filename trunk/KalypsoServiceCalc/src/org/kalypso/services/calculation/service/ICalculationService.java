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
import java.util.Date;

import javax.activation.DataHandler;

import org.kalypso.services.IKalypsoService;
import org.kalypso.services.calculation.job.ICalcJob;

/**
 * <p>
 * Interface für ICalculationService
 * </p>
 * 
 * <p>
 * Der ICalculationService ist ein Dienst um Berechnungen auszuführen.
 * </p>
 * <p>
 * Das ganze funktioniert ähnlich wie eine Druckerwarteschlange. Ein Auftrag
 * wird gesendet und in eine Liste gestellt, welche (normalerweise der Reihe
 * nach) abgearbeitet wird. Jeder Auftrag erhält dabei eine eindeutige ID. Ist
 * eine Auftrag fertig kann das Ergebnis abgeholt werden.
 * 
 * @author belger
 */
public interface ICalculationService extends Remote, IKalypsoService
{
  public long getSchemaValidity( final String namespace ) throws CalcJobServiceException;
  
  /**
   * Katalog-Service des Rechendienstes. Gibt für einen Namespace ein Schema
   * zurück. Sollte alle Schemata der von der jeweiligen Konfiguration
   * unterstüzten Modelle kennen.
   */
  public DataHandler getSchema( final String namespace ) throws CalcJobServiceException;

  /**
   * Gibt die IDs aller unterstützten JobTypen zurück.
   * 
   * @throws CalcJobServiceException
   */
  public String[] getJobTypes() throws CalcJobServiceException;

  /**
   * Gibt zurück, welche Eingaben für einen bestimmten Job-Typ benötigt werden.
   * 
   * @throws CalcJobServiceException
   */
  public CalcJobServerBean[] getRequiredInput( final String typeID ) throws CalcJobServiceException;

  /**
   * Gibt zurück, welche Ergebnisse von einem bestimmten Job-Typ geliefert
   * werden.
   * 
   * @throws CalcJobServiceException
   */
  public CalcJobServerBean[] getDeliveringResults( final String typeID )
      throws CalcJobServiceException;

  /**
   * Gibt den aktuellen Zustand aller vorhandenen {@link ICalcJob}zurück
   * 
   * @throws CalcJobServiceException
   */
  public CalcJobInfoBean[] getJobs() throws CalcJobServiceException;

  /** Gibt den aktuellen Zustand eines einzelnen {@link ICalcJob}zurück */
  public CalcJobInfoBean getJob( final String jobID ) throws CalcJobServiceException;

  /**
   * <p>
   * Erzeugt und startet einen neuen Auftrag (Job).
   * </p>
   * <p>
   * Auftrag wird in eine Warteliste gestellt und (abhängig von der
   * Implementation) baldmöglichst abgearbeitet.
   * </p>
   * 
   * @param typeID
   *          Rechentyp des neuen Auftrags.
   * @param description
   *          Menschenlesbare Beschreibung des Auftrags.
   * @param zipHandler
   *          Die eigentlichen Eingangsdaten für den Job. Der Inhalt muss ein
   *          JAR-Archiv sein.
   * @param input
   *          Die Eingabedateien, die der Client zur Verfügung stellt. Die
   *          relativen Pfade innerhalb der Benas beziehen sich auf Pfade
   *          innerhalb des Archivs.
   */
  public CalcJobInfoBean startJob( final String typeID, final String description,
      final DataHandler zipHandler, final CalcJobClientBean[] input,
      final CalcJobClientBean[] output ) throws CalcJobServiceException;

  /**
   * Stoppt einen Auftrag
   */
  public void cancelJob( final String jobID ) throws CalcJobServiceException;

  /**
   * Gibt die Ergebnisse eines Jobs zurück. Die Funktion kann jederzeit
   * aufgerufen werden, es werden alle im Moment verfügbaren Ergebnisse
   * zurückgeschickt. Der Job muss lediglich dafür sorgen, dass die Ergebnisse,
   * die er angegeben hat, auch tatsächlich da sind.
   * 
   * @param jobID
   *          Die id des Jobs, dessen Ergebnise geholt werden sollen.
   * @return Die Ergebnisse als ZIP Archiv, sollte in die Rechenvariante als
   *         Hauptverzeichnis entpackt werden.
   * @throws CalcJobServiceException
   */
  public DataHandler transferCurrentResults( final String jobID ) throws CalcJobServiceException;

  /**
   * Gibt zurück, welche Ergebniss (IDs) zur Zeit vorliegen.
   */
  public String[] getCurrentResults( final String jobID ) throws CalcJobServiceException;

  /**
   * <p>
   * Löscht einen Auftrag und alle temporär angelegten Daten vollständig.
   * <p>
   * <p>
   * Darf nur aufgerufen werden, wenn der Job gecanceled oder fertig ist
   * </p>
   */
  public void disposeJob( final String jobID ) throws CalcJobServiceException;
}
