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
package org.kalypso.ui.wizards.results;

import java.util.Date;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta.DOCUMENTTYPE;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta;

/**
 * @author Thomas Jung
 */
class Result1d2dMetaComparator extends ViewerComparator
{
  private static final int CALC_UNIT_RESULT_HIERARCHY = 10;

  private static final int STEP_RESULT_HIERARCHY = 30;

  private static final int DOCUMENT_RESULT_HIERARCHY = 50;

  private static final int TIN_WSP_DOCUMENT_RESULT_HIERARCHY = 70;

  private static final int TIN_DEPTH_DOCUMENT_RESULT_HIERARCHY = 71;

  private static final int TIN_VELO_DOCUMENT_RESULT_HIERARCHY = 72;

  private static final int TIN_SHEAR_DOCUMENT_RESULT_HIERARCHY = 73;

  private static final int TIN_DIFF_DOCUMENT_RESULT_HIERARCHY = 74;

  private static final int TIN_TERRAIN_DOCUMENT_RESULT_HIERARCHY = 75;

  private static final int NODE_DOCUMENT_RESULT_HIERARCHY = 76;

  private static final int HYDROGRAPH_DOCUMENT_RESULT_HIERARCHY = 77;

  private static final int LENGTHSECTION_DOCUMENT_RESULT_HIERARCHY = 78;

  private static final int CORE_DATA_DOCUMENT_RESULT_HIERARCHY = 79;

  private static final int LOG_DOCUMENT_RESULT_HIERARCHY = 80;

  @Override
  public int category( final Object element )
  {
    /* here, we return the hierarchy of the several element categories */

    if( element instanceof ICalcUnitResultMeta )
      return CALC_UNIT_RESULT_HIERARCHY;

    else if( element instanceof IStepResultMeta )
      return STEP_RESULT_HIERARCHY;

    else if( element instanceof IDocumentResultMeta )
    {
      final IDocumentResultMeta docResult = (IDocumentResultMeta)element;
      final DOCUMENTTYPE documentType = docResult.getDocumentType();
      switch( documentType )
      {
        case tinWsp:
          return TIN_WSP_DOCUMENT_RESULT_HIERARCHY;

        case tinDepth:
          return TIN_DEPTH_DOCUMENT_RESULT_HIERARCHY;

        case tinVelo:
          return TIN_VELO_DOCUMENT_RESULT_HIERARCHY;

        case tinShearStress:
          return TIN_SHEAR_DOCUMENT_RESULT_HIERARCHY;

        case tinDifference:
          return TIN_DIFF_DOCUMENT_RESULT_HIERARCHY;

        case nodes:
          return NODE_DOCUMENT_RESULT_HIERARCHY;

        case hydrograph:
          return HYDROGRAPH_DOCUMENT_RESULT_HIERARCHY;

        case lengthSection:
          return LENGTHSECTION_DOCUMENT_RESULT_HIERARCHY;

        case coreDataZip:
          return CORE_DATA_DOCUMENT_RESULT_HIERARCHY;

        case log:
          return LOG_DOCUMENT_RESULT_HIERARCHY;

        case tinTerrain:
          return TIN_TERRAIN_DOCUMENT_RESULT_HIERARCHY;

        default:
          return DOCUMENT_RESULT_HIERARCHY;
      }

    }

    else if( element instanceof IStepResultMeta )
      return STEP_RESULT_HIERARCHY;

    return 0;
  }

  @Override
  public int compare( final Viewer viewer, final Object e1, final Object e2 )
  {
    if( e1 instanceof IStepResultMeta && e2 instanceof IStepResultMeta )
    {
      final IStepResultMeta stepResult1 = (IStepResultMeta)e1;
      final IStepResultMeta stepResult2 = (IStepResultMeta)e2;

      /* sorting according the step time */

      final Date stepTime1 = stepResult1.getStepTime();
      final Date stepTime2 = stepResult2.getStepTime();

      if( stepTime1 == null )
        return -1;
      if( stepTime2 == null )
        return 1;

      if( stepTime1.after( stepTime2 ) )
        return 1;
      else if( stepTime1.before( stepTime2 ) )
        return -1;
      else
        return 0;
    }

    return super.compare( viewer, e1, e2 );
  }
}