/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.sobek.core;

import org.eclipse.osgi.util.NLS;

/**
 * @author kuch
 */
public class Messages extends NLS
{
  private static final String BUNDLE_NAME = "org.kalypso.model.wspm.sobek.core.messages"; //$NON-NLS-1$

  public static String Branch_0;

  public static String FNGmlUtils_0;

  public static String AbstractNode_0;

  public static String BoundaryNode_0;

  public static String Branch_1;

  public static String BranchMaker_0;

  public static String FNNodeUtils_1;

  public static String IBoundaryNode_0;

  public static String IBoundaryNode_4;

  public static String IBoundaryNodeLastfallCondition_4;

  public static String IBoundaryNodeLastfallCondition_5;

  public static String SobekModelMember_0;

  public static String SobekModelMember_1;

  public static String SobekModelMember_2;

  public static String SobekModelMember_3;
  public static String SobekModelMember_4;
  public static String SobekModelMember_5;


  public static String PiSobekModelUtils_0;
  public static String PiSobekModelUtils_1;
  public static String PiSobekModelUtils_2;
  public static String PiSobekModelUtils_3;
  public static String PiSobekModelUtils_4;
  public static String PiSobekModelUtils_5;

  static
  {
    // initialize resource bundle
    NLS.initializeMessages( BUNDLE_NAME, Messages.class );
  }

  private Messages( )
  {
  }
}
