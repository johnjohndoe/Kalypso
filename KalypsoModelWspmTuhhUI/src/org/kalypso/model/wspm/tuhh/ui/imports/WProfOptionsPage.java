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
package org.kalypso.model.wspm.tuhh.ui.imports;

import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.forms.widgets.ColumnLayout;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class WProfOptionsPage extends WizardPage
{
  private final WProfMarkerOptions m_markerOptions = new WProfMarkerOptions( this );

  private final WProfProfileStrategyOptions m_profileCreatorOptions = new WProfProfileStrategyOptions( this );

  public WProfOptionsPage( final String pageName )
  {
    super( pageName );
  }

  public WProfOptionsPage( final String pageName, final String title, final ImageDescriptor titleImage )
  {
    super( pageName, title, titleImage );
  }

  public WProfMarkerOptions getMarkerOptions( )
  {
    return m_markerOptions;
  }

  public WProfProfileStrategyOptions getProfileStrategyOptions( )
  {
    return m_profileCreatorOptions;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    panel.setLayout( new ColumnLayout() );

    final Group markerGroup = new Group( panel, SWT.NONE );
    markerGroup.setLayout( new FillLayout() );
    markerGroup.setText( Messages.getString("WProfOptionsPage_0") ); //$NON-NLS-1$
    m_markerOptions.createControl( markerGroup );

    final Group creatorGroup = new Group( panel, SWT.NONE );
    creatorGroup.setLayout( new FillLayout() );
    creatorGroup.setText( Messages.getString("WProfOptionsPage_1") ); //$NON-NLS-1$
    m_profileCreatorOptions.createControl( creatorGroup );

    setControl( panel );
  }

  public void validate( )
  {
    final IMessageProvider markerMessage = m_markerOptions.validate();
    if( markerMessage != null )
      setMessage( markerMessage.getMessage(), markerMessage.getMessageType() );
    else
      setMessage( null, IMessageProvider.NONE );
  }
}
