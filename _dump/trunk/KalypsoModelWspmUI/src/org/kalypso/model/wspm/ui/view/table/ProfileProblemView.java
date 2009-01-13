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
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.events.ExpansionAdapter;
import org.eclipse.ui.forms.events.ExpansionEvent;
import org.eclipse.ui.forms.events.HyperlinkAdapter;
import org.eclipse.ui.forms.events.HyperlinkEvent;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.ui.internal.ide.IDEInternalWorkbenchImages;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.MarkerIndex;
import org.kalypso.model.wspm.core.profil.reparator.IProfilMarkerResolution;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.observation.result.IRecord;

/**
 * @author kimwerner
 * @use the whole HeadClientArea to show the Profiles Problemmarkers
 */
public class ProfileProblemView
{

  final private FormToolkit m_toolkit;

  protected boolean errors_expanded = false;

  protected boolean warnings_expanded = false;

  protected boolean infos_expanded = false;
  
  protected final int m_MaxHeight;

  protected final ScrolledComposite m_scrolledComposite;

  public ProfileProblemView( final FormToolkit toolkit, final Composite parent, final int maxHeight )
  {

    m_toolkit = toolkit;
    m_MaxHeight = maxHeight;
    // Create the ScrolledComposite to scroll horizontally and vertically
    m_scrolledComposite = new ScrolledComposite( parent, SWT.V_SCROLL );
    m_scrolledComposite.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    final GridLayout scLayout = new GridLayout( 1, false );
    scLayout.marginWidth = 0;
    scLayout.marginHeight = 0;
    m_scrolledComposite.setLayout( scLayout );
    m_toolkit.adapt( m_scrolledComposite );
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
    Section result = null;
    if( markers.length > 1 )
    {
      final Section section = m_toolkit.createSection( parent, Section.TWISTIE );
      section.setLayout( new GridLayout( 2, false ) );
      section.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false,2,1 ) );
      section.setTitleBarForeground( section.getDisplay().getSystemColor( color ) );
      section.setToggleColor( section.getDisplay().getSystemColor( color ) );
      section.setText( markers.length + " " + text + org.kalypso.model.wspm.ui.i18n.Messages.getString("org.kalypso.model.wspm.ui.view.table.ProfileProblemView.0") ); //$NON-NLS-1$ //$NON-NLS-2$

      final Composite expanded_section = m_toolkit.createComposite( section );

      expanded_section.setLayout( new GridLayout( 2, false ) );
      expanded_section.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
      section.setClient( expanded_section );
      
      container = expanded_section;
      result = section;
    }

    for( final IMarker marker : markers )
    {
      final ImageHyperlink quickFix = m_toolkit.createImageHyperlink( container, SWT.WRAP );
      final IProfilMarkerResolution[] markerRes = getResolutions( marker );
      if( markerRes == null || markerRes.length < 1 )
      {
        quickFix.setToolTipText( org.kalypso.model.wspm.ui.i18n.Messages.getString("org.kalypso.model.wspm.ui.view.table.ProfileProblemView.1") ); //$NON-NLS-1$
        quickFix.setImage( JFaceResources.getResources().createImageWithDefault( IDEInternalWorkbenchImages.getImageDescriptor( IDEInternalWorkbenchImages.IMG_DLCL_QUICK_FIX_DISABLED ) ) );
      }
      else
      {
        final String toolTip = markerRes[0] == null ? null : markerRes[0].getDescription();
        quickFix.setToolTipText( toolTip == null ? org.kalypso.model.wspm.ui.i18n.Messages.getString("org.kalypso.model.wspm.ui.view.table.ProfileProblemView.2") : toolTip ); //$NON-NLS-1$
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
    return result;

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

  protected void updateParentSize()
  {
    final Control cmp = m_scrolledComposite.getContent();
    final Point size = cmp.computeSize( SWT.DEFAULT, SWT.DEFAULT );
    cmp.setSize( size );
    final int height = Math.min( size.y,50);
    ((GridData) m_scrolledComposite.getParent().getLayoutData()).heightHint = height;
    ((GridData) m_scrolledComposite.getLayoutData()).heightHint = height;
    m_scrolledComposite.getParent().getParent().layout();
  }

  private final boolean createSections( final Composite parent, final IProfil profil )
  {

    final MarkerIndex markerIndex = profil.getProblemMarker();
    if( !(markerIndex != null && markerIndex.hasMarkers()) )
    {
      return false;
    }
    final Section errSec = createSection( profil, parent, markerIndex.get( IMarker.SEVERITY_ERROR ), SWT.COLOR_RED, org.kalypso.model.wspm.ui.i18n.Messages.getString("org.kalypso.model.wspm.ui.view.table.ProfileProblemView.3") ); //$NON-NLS-1$
    final Section warnSec = createSection( profil, parent, markerIndex.get( IMarker.SEVERITY_WARNING ), SWT.COLOR_DARK_YELLOW, org.kalypso.model.wspm.ui.i18n.Messages.getString("org.kalypso.model.wspm.ui.view.table.ProfileProblemView.4") ); //$NON-NLS-1$
    final Section infSec = createSection( profil, parent, markerIndex.get( IMarker.SEVERITY_INFO ), SWT.COLOR_DARK_BLUE, org.kalypso.model.wspm.ui.i18n.Messages.getString("org.kalypso.model.wspm.ui.view.table.ProfileProblemView.5") ); //$NON-NLS-1$
    if( errSec != null )
    {
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
          updateParentSize(  );
        }

      } );
    }
    if( warnSec != null )
    {
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
          updateParentSize();
        }
      } );
    }
    if( infSec != null )
    {
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
          updateParentSize( );
        }
      } );
    }
    return true;
  }

  public final int updateSections( final IProfil profil)
  {

    if( m_scrolledComposite == null || m_scrolledComposite.isDisposed() )
      return -1;
    if( m_scrolledComposite.getContent() != null && !m_scrolledComposite.getContent().isDisposed() )
      m_scrolledComposite.getContent().dispose();

    if( profil == null )
      return -1;

    // Create a child composite to hold the controls
    Composite child = m_toolkit.createComposite( m_scrolledComposite );

    // Set the child as the scrolled content of the ScrolledComposite
    m_scrolledComposite.setContent( child );
    child.setLayout( new GridLayout( 2, false ) );
    child.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );

    if( createSections( (Composite)m_scrolledComposite.getContent(), profil ) )
    {
      final Point size = child.computeSize( SWT.DEFAULT, SWT.DEFAULT );
      child.setSize( size );
      ((GridData) (m_scrolledComposite.getLayoutData())).heightHint = Math.min( size.y, m_MaxHeight );
      return size.y;
    }
    return -1;
  }
}
