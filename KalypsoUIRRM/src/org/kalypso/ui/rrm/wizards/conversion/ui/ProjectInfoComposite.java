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
package org.kalypso.ui.rrm.wizards.conversion.ui;

import org.apache.commons.lang.StringUtils;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

/**
 * @author Gernot Belger
 */
public class ProjectInfoComposite extends Composite
{
  private static final String STR_TOOLTIP_COMMENT = "Projetbeschreibung (Benutzerdefiniert)";

  private final Text m_typeText;

  private final Text m_versionText;

  private IProjectDescription m_project;

  private final Text m_commentText;

  public ProjectInfoComposite( final Composite parent )
  {
    super( parent, SWT.NONE );

    setLayout( new GridLayout( 2, false ) );

    final Label commentLabel = new Label( this, SWT.NONE );
    commentLabel.setToolTipText( STR_TOOLTIP_COMMENT );
    commentLabel.setText( "Beschreibung" );
    m_commentText = new Text( this, SWT.READ_ONLY | SWT.BORDER );
    m_commentText.setToolTipText( STR_TOOLTIP_COMMENT );
    m_commentText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    new Label( this, SWT.NONE ).setText( "Modelltyp" );
    m_typeText = new Text( this, SWT.READ_ONLY | SWT.BORDER );
    m_typeText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    new Label( this, SWT.NONE ).setText( "Version" );
    m_versionText = new Text( this, SWT.READ_ONLY | SWT.BORDER );
    m_versionText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    updateControl();
  }

  public void setProject( final IProjectDescription project )
  {
    m_project = project;

    updateControl();
  }

  public IProjectDescription getProject( )
  {
    return m_project;
  }

  private void updateControl( )
  {
    if( m_project == null )
    {
      m_typeText.setText( "-" );
      m_versionText.setText( "-" );
      m_commentText.setText( "-" );
      return;
    }

    final String comment = m_project.getComment();
    if( StringUtils.isBlank( comment ) )
      m_commentText.setText( "<Nicht gesetzt>" );
    else
      m_commentText.setText( comment );

    // TODO: try to find out version + module type

    // TODO Auto-generated method stub

    m_versionText.setText( "<Unbekannte Version>" );
    m_typeText.setText( "<Unbekannter Modelltyp>" );

  }
}
