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

import org.apache.commons.lang.StringUtils;
import org.eclipse.core.resources.IMarker;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.events.ExpansionAdapter;
import org.eclipse.ui.forms.events.ExpansionEvent;
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

  protected boolean errors_expanded = false;

  protected boolean warnings_expanded = false;

  protected boolean infos_expanded = false;

  public ProfileProblemView( final FormToolkit toolkit, final Form form )
  {

    m_form = form;
    m_toolkit = toolkit;
  }

  private IProfilMarkerResolution[] getResolutions( final IMarker marker )
  {
    final String resArray = marker.getAttribute( IValidatorMarkerCollector.MARKER_ATTRIBUTE_QUICK_FIX_RESOLUTIONS, (String) null );

    final String[] resolutions = StringUtils.split( resArray, '\u0000' );
    final IProfilMarkerResolution[] markerRes = new IProfilMarkerResolution[resolutions == null ? 0 : resolutions.length];
    for( int i = 0; i < markerRes.length; i++ )
    {
      final IProfilMarkerResolution markerResolution = KalypsoModelWspmCoreExtensions.getReparatorRule( resolutions[i] );
      if( markerResolution != null )
        markerRes[i] = markerResolution;
    }
    return markerRes;
  }

  private final Section createSection( final IProfil profil, final Composite parent, final IMarker[] markers, final int color, final String text )
  {
    Composite container = parent;
    Section section = null;
    if( markers.length > 1 )
    {
      section = m_toolkit.createSection( parent, Section.TWISTIE );
      section.setLayout( new GridLayout() );
      section.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true, 2, 1 ) );
      section.setTitleBarForeground( section.getDisplay().getSystemColor( color ) );
      section.setToggleColor( section.getDisplay().getSystemColor( color ) );
      section.setText( markers.length + " " + text + Messages.TableView_5 ); //$NON-NLS-1$

      final Composite expanded_section = m_toolkit.createComposite( section );
      expanded_section.setLayout( new GridLayout( 2, false ) );
      expanded_section.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );

      container = expanded_section;
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
    return section;
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
    final Section errSec = createSection( profil, container, markerIndex.get( IMarker.SEVERITY_ERROR ), SWT.COLOR_RED, Messages.TableView_1 );
    final Section warnSec = createSection( profil, container, markerIndex.get( IMarker.SEVERITY_WARNING ), SWT.COLOR_DARK_YELLOW, Messages.TableView_2 );
    final Section infSec = createSection( profil, container, markerIndex.get( IMarker.SEVERITY_INFO ), SWT.COLOR_DARK_BLUE, Messages.TableView_3 );
    if( errSec != null )
    {
      if( errSec.isExpanded() != errors_expanded )
        errSec.setExpanded( errors_expanded );
      errSec.addExpansionListener( new ExpansionAdapter()
      {

        /**
         * @see org.eclipse.ui.forms.events.ExpansionAdapter#expansionStateChanged(org.eclipse.ui.forms.events.ExpansionEvent)
         */
        @Override
        public void expansionStateChanged( ExpansionEvent e )
        {
          errors_expanded = errSec.isExpanded();
        }
      } );
    }
    if( warnSec != null )
    {
      if( warnSec.isExpanded() != warnings_expanded )
        warnSec.setExpanded( warnings_expanded );
      warnSec.addExpansionListener( new ExpansionAdapter()
      {

        /**
         * @see org.eclipse.ui.forms.events.ExpansionAdapter#expansionStateChanged(org.eclipse.ui.forms.events.ExpansionEvent)
         */
        @Override
        public void expansionStateChanged( ExpansionEvent e )
        {
          warnings_expanded = warnSec.isExpanded();
        }
      } );
    }
    if( infSec != null )
    {
      if( infSec.isExpanded() != infos_expanded )
        infSec.setExpanded( infos_expanded );
      infSec.addExpansionListener( new ExpansionAdapter()
      {

        /**
         * @see org.eclipse.ui.forms.events.ExpansionAdapter#expansionStateChanged(org.eclipse.ui.forms.events.ExpansionEvent)
         */
        @Override
        public void expansionStateChanged( ExpansionEvent e )
        {
          infos_expanded = infSec.isExpanded();
        }
      } );
    }
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
