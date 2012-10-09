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
package org.kalypso.model.wspm.pdb.gaf;

/**
 * @author Gernot Belger
 */
public interface IGafConstants
{
  String POINT_KIND_GAF = "GAF"; //$NON-NLS-1$

  /* CODES */
  String CODE_PP = "PP"; //$NON-NLS-1$

  String CODE_PA = "PA"; //$NON-NLS-1$

  String CODE_PE = "PE";//$NON-NLS-1$

  String CODE_LU = "LU";//$NON-NLS-1$

  String CODE_RU = "RU";//$NON-NLS-1$

  String CODE_LBOK = "LBOK";//$NON-NLS-1$

  String CODE_RBOK = "RBOK";//$NON-NLS-1$

  String CODE_UKAN = "UKAN"; //$NON-NLS-1$

  String CODE_UKEN = "UKEN"; //$NON-NLS-1$

  String CODE_UKPP = "UKPP"; //$NON-NLS-1$

  String CODE_OKAN = "OKAN"; //$NON-NLS-1$

  String CODE_OKEN = "OKEN"; //$NON-NLS-1$

  String CODE_OKPP = "OKPP"; //$NON-NLS-1$

  String CODE_WS = "WS"; //$NON-NLS-1$

  String CODE_KRUK = "KRUK"; //$NON-NLS-1$

  String CODE_KRFS = "KRFS"; //$NON-NLS-1$

  String CODE_EIUK = "EIUK"; //$NON-NLS-1$

  String CODE_EIFS = "EIFS"; //$NON-NLS-1$

  String CODE_MAUK = "MAUK"; //$NON-NLS-1$

  String CODE_MAFS = "MAFS"; //$NON-NLS-1$

  /* HYK */
  String HYK_PA = CODE_PA;

  String HYK_PE = CODE_PE;

  String HYK_LBOK = CODE_LBOK;

  String HYK_RBOK = CODE_RBOK;

  String HYK_LU = CODE_LU;

  String HYK_RU = CODE_RU;

  char HYK_CODE_SEPARATOR = ','; //$NON-NLS-1$

  /* special building codes */
  String KIND_TR = "TR"; //$NON-NLS-1$

  String KIND_ENERGYLOSS = "ENERGYLOSS"; //$NON-NLS-1$

  String KIND_SINUOSITAET = "SINUOSITAET"; //$NON-NLS-1$

  /* special metadata for profile objects */
  String PART_NAME = "PART_NAME"; //$NON-NLS-1$

  String PART_TYPE = "PART_TYPE"; //$NON-NLS-1$

  /** profile object metadata key for discharge of 2d waterlevels */
  String METADATA_WATERLEVEL_DISCHARGE = "WATERLEVEL_DISCHARGE"; //$NON-NLS-1$
}