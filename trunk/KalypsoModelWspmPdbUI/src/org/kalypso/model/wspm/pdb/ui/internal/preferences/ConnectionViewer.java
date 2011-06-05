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
package org.kalypso.model.wspm.pdb.ui.internal.preferences;

import org.apache.commons.lang.StringUtils;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;
import org.kalypso.contribs.eclipse.jface.wizard.IUpdateable;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.ui.internal.admin.ConnectionAdminControl;
import org.kalypso.model.wspm.pdb.ui.internal.content.ConnectionContentControl;
import org.kalypso.model.wspm.pdb.ui.internal.content.filter.StateFilterControl;
import org.kalypso.model.wspm.pdb.ui.internal.content.filter.WaterBodyFilterControl;
import org.kalypso.model.wspm.pdb.ui.internal.wspm.PdbWspmProject;

/**
 * @author Gernot Belger
 */
public class ConnectionViewer extends Composite
{
  private final IUpdateable m_updateable = new IUpdateable()
  {
    @Override
    public void update( )
    {
      reload( null );
    }
  };

  private final IPdbConnection m_connection;

  private ConnectionContentControl m_contentViewer;

  private final PdbWspmProject m_project;

  public ConnectionViewer( final FormToolkit toolkit, final Composite parent, final IPdbConnection connection, final PdbWspmProject project )
  {
    super( parent, SWT.NONE );

    m_connection = connection;
    m_project = project;

    toolkit.adapt( this );
    GridLayoutFactory.fillDefaults().applyTo( this );

    createAdminGroup( toolkit, this ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    createPdbView( toolkit, this ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    final StructuredViewer contentViewer = m_contentViewer.getViewer();
    createSearchControls( toolkit, this, contentViewer ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
  }

  private Control createAdminGroup( final FormToolkit toolkit, final Composite parent )
  {
    final Section section = toolkit.createSection( parent, Section.DESCRIPTION | Section.TITLE_BAR | Section.TWISTIE );
    // TODO: nur zeigen, wenn der user admin rechte hat
    section.setText( "Administration" );
    section.setDescription( "This section allows to administrate the cross section database." );
    section.setLayout( new FillLayout() );

    final ConnectionAdminControl adminControl = new ConnectionAdminControl( toolkit, section, m_connection, m_updateable );

    section.setClient( adminControl );

    return section;
  }

  private Control createPdbView( final FormToolkit toolkit, final Composite parent )
  {
    final Section section = toolkit.createSection( parent, Section.DESCRIPTION | Section.TITLE_BAR );
    section.setText( "Content" );
    section.setDescription( "Contents of the cross section database." );
    section.setLayout( new FillLayout() );

    m_contentViewer = new ConnectionContentControl( toolkit, section, m_connection, m_project );

    section.setClient( m_contentViewer );

    return section;
  }

  private Control createSearchControls( final FormToolkit toolkit, final Composite parent, final StructuredViewer viewer )
  {
    final Section section = toolkit.createSection( parent, Section.TITLE_BAR | Section.DESCRIPTION | Section.TWISTIE );
    section.setText( "Search" );
    section.setDescription( "Edit search fields to filter visible items." );
    section.setLayout( new FillLayout() );

    final Composite panel = toolkit.createComposite( section );
    GridLayoutFactory.swtDefaults().applyTo( panel );

    section.setClient( panel );

    final WaterBodyFilterControl waterFilterControl = new WaterBodyFilterControl( toolkit, panel );
    waterFilterControl.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    waterFilterControl.setViewer( viewer );

    toolkit.createLabel( panel, StringUtils.EMPTY, SWT.HORIZONTAL | SWT.SEPARATOR ).setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final StateFilterControl stateFilterControl = new StateFilterControl( toolkit, panel );
    stateFilterControl.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    stateFilterControl.setViewer( viewer );

    return section;
  }

  public void reload( final String stateToSelect )
  {
    if( m_contentViewer != null )
      m_contentViewer.refresh( stateToSelect );
  }
}