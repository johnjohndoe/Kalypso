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
package org.kalypso.model.wspm.tuhh.ui.light.documents;

import javax.xml.bind.JAXBException;
import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.PartInitException;
import org.kalypso.featureview.views.FeatureView;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.WspmFixation;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.ogc.gml.featureview.maker.CachedFeatureviewFactory;
import org.kalypso.template.featureview.FeatureviewType;
import org.kalypso.ui.catalogs.FeatureTypeFeatureviewCatalog;

/**
 * @author Gernot Belger
 */
public class WspmLightFeatureView extends FeatureView
{
  public static final String WSPMLIGHTFEATUREVIEW_ID = "org.kalypso.model.wspm.tuhh.ui.light.WspmFeatureView"; //$NON-NLS-1$

  private static final String ATTACHMENT_STYLE = "documents"; //$NON-NLS-1$

  @Override
  public void init( final IViewSite site ) throws PartInitException
  {
    super.init( site );

    /* Replace 'default' gft's with 'attachment' gft's */
    addView( WspmWaterBody.QNAME );
    addView( WspmFixation.QNAME_FEATURE_WSPM_FIXATION );
    addView( TuhhReach.QNAME_TUHH_REACH );
    addView( TuhhReachProfileSegment.QNAME_PROFILEREACHSEGMENT );
    addView( IProfileFeature.QN_PROFILE );
  }

  private void addView( final QName qname )
  {
    try
    {
      final FeatureviewType featureview = FeatureTypeFeatureviewCatalog.getFeatureview( null, qname, ATTACHMENT_STYLE );
      if( featureview == null )
        return;

      final CachedFeatureviewFactory factory = getCachedFeatureViewFactory();
      factory.addView( featureview );
    }
    catch( final JAXBException e )
    {
      final String msg = String.format( "Failed to configure documents feature view for: '%s'", qname );
      final IStatus status = new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getID(), msg, e );
      KalypsoModelWspmTuhhUIPlugin.getDefault().getLog().log( status );
    }
  }
}