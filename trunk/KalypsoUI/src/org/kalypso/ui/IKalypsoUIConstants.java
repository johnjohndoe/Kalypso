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
package org.kalypso.ui;

/**
 * Constants for the Kalypso UI.
 * 
 * Not intended to be implemented nor extended.
 * 
 * @author schlienger
 */
public interface IKalypsoUIConstants
{
  /** Observation Diagram View identifier (value <code>org.kalypso.ogc.sensor.view.DiagramViewPart</code>) */
  public final static String ID_OBSDIAGRAM_VIEW = "org.kalypso.ogc.sensor.view.DiagramViewPart"; //$NON-NLS-1$

  /** Observation Table View identifier (value <code>org.kalypso.ogc.sensor.view.TableViewPart</code>) */
  public final static String ID_OBSTABLE_VIEW = "org.kalypso.ogc.sensor.view.TableViewPart"; //$NON-NLS-1$
  
  /** Repository View identifier (value <code>org.kalypso.ui.repository.view.RepositoryExplorerPart</code>) */
  public final static String ID_REPOSITORY_VIEW = "org.kalypso.ui.repository.view.RepositoryExplorerPart";

  public final static String ID_PROGNOSE_VIEW = "org.kalypso.view.prognose";

  public static final String MODELER_PERSPECTIVE = "org.kalypso.ui.perspectives.ModelerPerspectiveFactory"; //$NON-NLS-1$

  public static final String REPOSITORY_PERSPECTIVE = "org.kalypso.ui.perspectives.ObservationRepositoryPerspectiveFactory"; //$NON-NLS-1$

  public static final String PROGNOSE_PERSPECTIVE = "org.kalypso.ui.perspectives.PrognosePerspective"; //$NON-NLS-1$
  
  /** Constant for all Kalypso data import wizards (Extenstion point schema org.kalypso.ui.wizard.dataImportWizard.exsd)*/
  public static final String PL_IMPORT = "dataimportwizard";
}