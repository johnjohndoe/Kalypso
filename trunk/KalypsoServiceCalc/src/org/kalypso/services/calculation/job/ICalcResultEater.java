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
package org.kalypso.services.calculation.job;

import java.io.File;

import javax.activation.DataHandler;

import org.kalypso.services.calculation.service.CalcJobServiceException;

/**
 * <p>
 * Sammelt die Ergebnisse des
 * {@link org.kalypso.services.calculation.job.ICalcJob}.
 * </p>
 * 
 * <p>
 * Daneben sammelt der Dateien, die sp�ter gel�scht werden sollen, das ist aber
 * nur f�r internen gebrauch gedacht.
 * </p>
 * 
 * @author belger
 */
public interface ICalcResultEater
{
  public void addResult( final String id, final File file ) throws CalcJobServiceException;

  /** Gibt zur�ck, welche Ergebnisse momentan verf�gbar sind. */
  public String[] getCurrentResults();

  /**
   * Verpackt die aktuell vorliegenden Ergebnisse
   * 
   * @throws CalcJobServiceException
   */
  public DataHandler packCurrentResults() throws CalcJobServiceException;

  /**
   * Dateien sammeln, um diese sp�ter zu l�schen. Nur f�r interen Gebrauch
   * gedacht.
   */
  public void addFile( final File file );

  /**
   * L�scht alle bisher hinzugef�gten Dateien. Wenns Verzeichnisse sind, wird
   * auch der Inhalt rekursiv gel�scht.
   */
  public void disposeFiles();
}
