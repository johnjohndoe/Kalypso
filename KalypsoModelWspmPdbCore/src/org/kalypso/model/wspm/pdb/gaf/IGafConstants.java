/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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

  char HYK_CODE_SEPARATOR = ','; //$NON-NLS-1$

  /* special building codes */
  String KIND_TR = "TR"; //$NON-NLS-1$

  String KIND_ENERGYLOSS = "ENERGYLOSS"; //$NON-NLS-1$

  String KIND_SINUOSITAET = "SINUOSITAET"; //$NON-NLS-1$

  /** part type for 2d waterlevels */
  String KIND_W2D = "W2D"; //$NON-NLS-1$

  /* special metadata for profile objects */
  String PART_NAME = "PART_NAME"; //$NON-NLS-1$

  /** profile object metadata key for discharge of 2d waterlevels */
  String METADATA_WATERLEVEL_DISCHARGE = "WATERLEVEL_DISCHARGE"; //$NON-NLS-1$
}