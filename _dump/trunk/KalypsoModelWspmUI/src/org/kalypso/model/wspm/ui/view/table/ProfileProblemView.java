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
package org.kalypso.model.wspm.ui.view.table;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.eclipse.core.resources.IMarker;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.events.HyperlinkAdapter;
import org.eclipse.ui.forms.events.HyperlinkEvent;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.ui.internal.ide.IDEInternalWorkbenchImages;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.MarkerIndex;
import org.kalypso.model.wspm.core.profil.reparator.IProfilMarkerResolution;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.ui.Messages;
import org.kalypso.observation.result.IRecord;

/**
 * @author kimwerner
 * @use the whole HeadClientArea to show the Profiles Problemmarkers
 */
public class ProfileProblemView
{

  final private FormToolkit m_toolkit;

  final private Form m_form;

  public ProfileProblemView( final FormToolkit toolkit, final Form form )
  {

    m_form = form;
    m_toolkit = toolkit;
  }

  private IProfilMarkerResolution[] getResolutions( final IMarker marker )
  {
    final String resArray = marker.getAttribute( IValidatorMarkerCollector.MARKER_ATTRIBUTE_QUICK_FIX_RESOLUTIONS, (String) null );

    final String[] resolutions = StringUtils.split( resArray, '\u0000' );
    final IProfilMarkerResolution[] markerRes = new IProfilMarkerResolution[ resolutions == null ? 0 : resolutions.length ];
    for( int i = 0; i < markerRes.length; i++ )
    {
      final IProfilMarkerResolution markerResolution = KalypsoModelWspmCoreExtensions.getReparatorRule( resolutions[i] );
      if( markerResolution != null )
        markerRes[i]= markerResolution ;
    }
    return markerRes;
  }

  private final void createSection( final IProfil profil, final Composite parent, final IMarker[] markers, final int color, final String text )
  {
    Composite container = parent;
    if( markers.length > 1 )
    {
      final Section section = m_toolkit.createSection( parent, Section.TWISTIE );
      section.setLayout( new GridLayout() );
      section.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true, 2, 1 ) );
      section.setTitleBarForeground( section.getDisplay().getSystemColor( color ) );
      section.setToggleColor( section.getDisplay().getSystemColor( color ) );

      section.setText( markers.length + " " + text + Messages.TableView_5 ); //$NON-NLS-1$
      final Composite expanded = m_toolkit.createComposite( section );
      expanded.setLayout( new GridLayout( 2, false ) );
      expanded.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
      container = expanded;
      section.setClient( container );
    }

    for( final IMarker marker : markers )
    {

      final ImageHyperlink quickFix = m_toolkit.createImageHyperlink( container, SWT.WRAP );
      final IProfilMarkerResolution[] markerRes = getResolutions( marker );
      if( markerRes == null || markerRes.length < 1 )
      {
        quickFix.setToolTipText( Messages.TableView_10 );
        quickFix.setImage( JFaceResources.getResources().createImageWithDefault( IDEInternalWorkbenchImages.getImageDescriptor( IDEInternalWorkbenchImages.IMG_DLCL_QUICK_FIX_DISABLED ) ) );
      }
      else
      {
        final String toolTip = markerRes[0] == null ? null : markerRes[0].getDescription();
        quickFix.setToolTipText( toolTip == null ? Messages.TableView_11 : toolTip );
        quickFix.setImage( JFaceResources.getResources().createImageWithDefault( IDEInternalWorkbenchImages.getImageDescriptor( IDEInternalWorkbenchImages.IMG_ELCL_QUICK_FIX_ENABLED ) ) );
        quickFix.addHyperlinkListener( new HyperlinkAdapter()
        {

          /**
           * @see org.eclipse.ui.forms.events.HyperlinkAdapter#linkActivated(org.eclipse.ui.forms.events.HyperlinkEvent)
           */
          @Override
          public void linkActivated( HyperlinkEvent e )
          {
            for( final IProfilMarkerResolution profilMarkerResolution : markerRes )
            {
              if( profilMarkerResolution != null )
                profilMarkerResolution.resolve( profil );
            }
          }
        } );
      }
      final ImageHyperlink link = m_toolkit.createImageHyperlink( container, SWT.WRAP );
      link.addHyperlinkListener( new HyperlinkAdapter()
      {

        /**
         * @see org.eclipse.ui.forms.events.HyperlinkAdapter#linkActivated(org.eclipse.ui.forms.events.HyperlinkEvent)
         */
        @Override
        public void linkActivated( HyperlinkEvent e )
        {

          if( profil == null )
            return;

          final int pointPos = marker.getAttribute( IValidatorMarkerCollector.MARKER_ATTRIBUTE_POINTPOS, -1 );
          if( pointPos < 0 )
            return;
          final IRecord record = profil.getPoint( pointPos );
          profil.setActivePoint( record );
        }
      } );
      link.setText( marker.getAttribute( IMarker.MESSAGE, "" ) ); //$NON-NLS-1$
      link.setImage( JFaceResources.getResources().createImageWithDefault( getImageDescriptor( marker ) ) );
      link.setForeground( container.getDisplay().getSystemColor( color ) );
    }

  }

  private ImageDescriptor getImageDescriptor( final IMarker marker )
  {

    switch( marker.getAttribute( IMarker.SEVERITY, 0 ) )
    {
      case IMarker.SEVERITY_ERROR:
        return IDEInternalWorkbenchImages.getImageDescriptor( IDEInternalWorkbenchImages.IMG_OBJS_ERROR_PATH );
      case IMarker.SEVERITY_WARNING:
        return IDEInternalWorkbenchImages.getImageDescriptor( IDEInternalWorkbenchImages.IMG_OBJS_WARNING_PATH );
      case IMarker.SEVERITY_INFO:
        return IDEInternalWorkbenchImages.getImageDescriptor( IDEInternalWorkbenchImages.IMG_OBJS_INFO_PATH );
      default:
        return null;
    }
  }

  private final Composite createSections( final IProfil profil )
  {
    final MarkerIndex markerIndex = profil.getProblemMarker();
    if( !(markerIndex != null && markerIndex.hasMarkers()) )
      return null;

    final Composite container = m_toolkit.createComposite( m_form.getHead() );
    createSection( profil, container, markerIndex.get( IMarker.SEVERITY_ERROR ), SWT.COLOR_RED, Messages.TableView_1 );
    createSection( profil, container, markerIndex.get( IMarker.SEVERITY_WARNING ), SWT.COLOR_DARK_YELLOW, Messages.TableView_2 );
    createSection( profil, container, markerIndex.get( IMarker.SEVERITY_INFO ), SWT.COLOR_DARK_BLUE, Messages.TableView_3 );
    container.setLayout( new GridLayout( 2, false ) );
    container.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
    m_form.setHeadClient( container );
    return container;
  }

  public final void updateSections( final IProfil profil )
  {
    if( m_form == null || m_form.isDisposed() )
      return;
    if( m_form.getHeadClient() != null )
    {
      m_form.getHeadClient().dispose();
      m_form.setHeadClient( null );
    }
    if( profil == null )
      return;
    Composite form = createSections( profil );

    if( form != null )
    {
      form.setLayout( new GridLayout( 2, false ) );
      form.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
      m_form.setHeadClient( form );
    }

  }

}
