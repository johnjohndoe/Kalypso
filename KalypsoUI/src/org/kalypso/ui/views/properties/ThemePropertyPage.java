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
package org.kalypso.ui.views.properties;

import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbenchPropertyPage;
import org.eclipse.ui.dialogs.PropertyPage;
import org.kalypso.commons.i18n.I10nString;
import org.kalypso.ogc.gml.IKalypsoTheme;

/**
 * This is a page for showing some properties of a theme.
 * 
 * @author Gernot Belger
 * @author Holger Albert
 */
public class ThemePropertyPage extends PropertyPage implements IWorkbenchPropertyPage
{
  /**
   * The name of the theme.
   */
  private String m_themeName;

  /**
   * @see org.eclipse.jface.preference.PreferencePage#createContents(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createContents( final Composite parent )
  {
    /* Get the theme. */
    final IKalypsoTheme theme = getTheme();

    final Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayout( new GridLayout( 2, false ) );

    if( theme == null )
    {
      // todo: show some error message
      return composite;
    }

    final Label label = new Label( composite, SWT.NONE );
    label.setText( "Name:" );

    final Text text = new Text( composite, SWT.NONE );
    text.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    text.setText( theme.getLabel() );
    text.addModifyListener( new ModifyListener()
    {
      public void modifyText( final ModifyEvent e )
      {
        final String name = text.getText();
        setThemeName( name );
      }
    } );

    return composite;
  }

  /**
   * This function returns the theme.
   * 
   * @return The theme.
   */
  private IKalypsoTheme getTheme( )
  {
    final IAdaptable element = getElement();
    final IKalypsoTheme theme = (IKalypsoTheme) (element instanceof IKalypsoTheme ? element : element.getAdapter( IKalypsoTheme.class ));

    return theme;
  }

  /**
   * @see org.eclipse.jface.preference.PreferencePage#performOk()
   */
  @Override
  public boolean performOk( )
  {
    final IKalypsoTheme theme = getTheme();
    if( theme != null )
      theme.setName( new I10nString( m_themeName ) );

    return super.performOk();
  }

  /**
   * This function sets the name of the theme.
   * 
   * @param name
   *            The name of the theme.
   */
  protected void setThemeName( final String name )
  {
    m_themeName = name;

    setValid( true );
  }
}
