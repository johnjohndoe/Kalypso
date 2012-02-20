/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.hydrology.project;

/**
 * Constants relevant for the structure of a na model calc case.<br/>
 * Most pathes are relative to the calc case.
 * 
 * @author Gernot Belger
 * @deprecated Use {@link CalcCaseAccessor} instead.
 */
@Deprecated
public interface INaCalcCaseConstants
{
  String EXPERT_CONTROL_FILE = "expertControl.gml"; //$NON-NLS-1$

  String EXPERT_CONTROL_PATH = INaProjectConstants.FOLDER_MODELS + "/" + EXPERT_CONTROL_FILE; //$NON-NLS-1$

  String CALCULATION_GML_FILE = "calculation.gml"; //$NON-NLS-1$

  String CALCULATION_GML_PATH = INaProjectConstants.FOLDER_MODELS + "/" + "calculation.gml"; //$NON-NLS-1$

  String ANFANGSWERTE_DIR = "Anfangswerte"; //$NON-NLS-1$

  String ANFANGSWERTE_FILE = "lzsim.gml"; //$NON-NLS-1$

  String KLIMA_DIR = "Klima"; //$NON-NLS-1$

  String NIEDERSCHLAG_DIR = "Niederschlag"; //$NON-NLS-1$

  String PEGEL_DIR = "Pegel"; //$NON-NLS-1$

  String ERGEBNISSE_DIR = "Ergebnisse"; //$NON-NLS-1$

  String AKTUELL_DIR = ERGEBNISSE_DIR + '/' + "Aktuell"; //$NON-NLS-1$

  String CATCHMENT_FILE = "catchmentModels.gml"; //$NON-NLS-1$
}
