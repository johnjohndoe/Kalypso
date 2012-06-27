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
package org.kalypso.kalypso1d2d.pjt.views.contentprov;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.model.WorkbenchAdapter;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectImages.DESCRIPTORS;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta.DOCUMENTTYPE;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta.STEPTYPE;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;

/**
 * @author Thomas Jung
 *
 * Adapter class that defines the label and images for the result view.
 *
 */
public class ResultMetaWorkbenchAdapter extends WorkbenchAdapter
{
  /**
   * @see org.eclipse.ui.model.WorkbenchAdapter#getChildren(java.lang.Object)
   */
  @Override
  public Object[] getChildren( final Object object )
  {
    if( object instanceof IResultMeta )
      return ((IResultMeta) object).getChildren().toArray();

    return super.getChildren( object );
  }

  @Override
  public Object getParent( final Object object )
  {
    if( object instanceof IResultMeta )
      return ((IResultMeta) object).getOwner();

    return super.getParent( object );
  }

  @Override
  public String getLabel( final Object object )
  {
    if( object instanceof IResultMeta )
      return ((IResultMeta) object).getName();

    return super.getLabel( object );
  }

  /**
   * @see org.eclipse.ui.model.WorkbenchAdapter#getImageDescriptor(java.lang.Object)
   */
  @Override
  public ImageDescriptor getImageDescriptor( final Object object )
  {
    if( object instanceof IScenarioResultMeta )
      return Kalypso1d2dProjectPlugin.getImageProvider().getImageDescriptor( DESCRIPTORS.RESULT_META_SCENARIO );
    else if( object instanceof ICalcUnitResultMeta )
      return Kalypso1d2dProjectPlugin.getImageProvider().getImageDescriptor( DESCRIPTORS.RESULT_META_CALC_UNIT );
    else if( object instanceof IStepResultMeta )
    {
      /* separate icons for separate types */
      return getStepResultImage( object );
    }
    else if( object instanceof IDocumentResultMeta )
    {
      /* separate icons for separate types */
      return getDocumentResultImage( object );
    }
    else
      return Kalypso1d2dProjectPlugin.getImageProvider().getImageDescriptor( DESCRIPTORS.RESULT_META_ERROR );

  }

  private ImageDescriptor getDocumentResultImage( final Object object )
  {
    final DOCUMENTTYPE documentType = ((IDocumentResultMeta) object).getDocumentType();
    switch( documentType )
    {
      case nodes:
        return Kalypso1d2dProjectPlugin.getImageProvider().getImageDescriptor( DESCRIPTORS.RESULT_META_DOCUMENT_NODES );

      case tinDepth:
        return Kalypso1d2dProjectPlugin.getImageProvider().getImageDescriptor( DESCRIPTORS.RESULT_META_DOCUMENT_DEPTH );

      case tinVelo:
        return Kalypso1d2dProjectPlugin.getImageProvider().getImageDescriptor( DESCRIPTORS.RESULT_META_DOCUMENT_VELO );

      case tinWsp:
        return Kalypso1d2dProjectPlugin.getImageProvider().getImageDescriptor( DESCRIPTORS.RESULT_META_DOCUMENT_WSP );

      case tinShearStress:
        return Kalypso1d2dProjectPlugin.getImageProvider().getImageDescriptor( DESCRIPTORS.RESULT_META_DOCUMENT_TIN );

      case tinDifference:
        return Kalypso1d2dProjectPlugin.getImageProvider().getImageDescriptor( DESCRIPTORS.RESULT_META_DOCUMENT_DIFFERENCES );

      case lengthSection:
        return Kalypso1d2dProjectPlugin.getImageProvider().getImageDescriptor( DESCRIPTORS.RESULT_META_DOCUMENT_LENGTH_SECTION );

      case hydrograph:
        return Kalypso1d2dProjectPlugin.getImageProvider().getImageDescriptor( DESCRIPTORS.RESULT_META_DOCUMENT_HYDRO );

      case log:
        return Kalypso1d2dProjectPlugin.getImageProvider().getImageDescriptor( DESCRIPTORS.RESULT_META_DOCUMENT_LOG );

      case coreDataZip:
        return Kalypso1d2dProjectPlugin.getImageProvider().getImageDescriptor( DESCRIPTORS.RESULT_META_DOCUMENT_ZIP );

      default:
        return Kalypso1d2dProjectPlugin.getImageProvider().getImageDescriptor( DESCRIPTORS.RESULT_META_ERROR );
    }
  }

  private ImageDescriptor getStepResultImage( final Object object )
  {
    final STEPTYPE stepType = ((IStepResultMeta) object).getStepType();
    switch( stepType )
    {
      case steady:
        return Kalypso1d2dProjectPlugin.getImageProvider().getImageDescriptor( DESCRIPTORS.RESULT_META_STEP_STEADY );
      case unsteady:
        return Kalypso1d2dProjectPlugin.getImageProvider().getImageDescriptor( DESCRIPTORS.RESULT_META_STEP_UNSTEADY );
      case maximum:
        return Kalypso1d2dProjectPlugin.getImageProvider().getImageDescriptor( DESCRIPTORS.RESULT_META_STEP_MAX );
      case qSteady:
        return Kalypso1d2dProjectPlugin.getImageProvider().getImageDescriptor( DESCRIPTORS.RESULT_META_STEP_QSTEADY );
      default:
        return Kalypso1d2dProjectPlugin.getImageProvider().getImageDescriptor( DESCRIPTORS.RESULT_META_ERROR );
    }
  }
}
