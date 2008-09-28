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
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.command.CompositeCommand;
import org.kalypso.ogc.gml.map.themes.KalypsoWMSTheme;

/**
 * This is a page for showing properties of a {@link KalypsoWMSTheme}.<br>
 * TODO: we are directly displaying SOURCE here, but this is of course not really what we want<br>
 * Instead, we need to parse the source attribute (we should use a data class for this, and reuse it in the WMS Theme),
 * then show the different parts of source separately.
 * 
 * @author Gernot Belger
 */
public class WMSThemePropertyPage extends PropertyPage implements IWorkbenchPropertyPage
{
  private String m_themeSource = null;

  /**
   * @see org.eclipse.jface.preference.PreferencePage#createContents(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createContents( final Composite parent )
  {
    /* Get the theme. */
    final KalypsoWMSTheme theme = getTheme();

    final Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayout( new GridLayout( 2, false ) );

    if( theme == null )
    {
      // todo: show some error message
      return composite;
    }

    /* Theme name */
    final Label sourceLabel = new Label( composite, SWT.NONE );
    sourceLabel.setText( Messages.getString( "org.kalypso.ui.views.properties.WMSThemePropertyPage.sourceLabel" ) ); //$NON-NLS-1$

    // Yet read only, as WMS-Theme does not support changing the source yet
    final Text sourceText = new Text( composite, SWT.READ_ONLY | SWT.BORDER );
    final GridData sourceData = new GridData( SWT.FILL, SWT.CENTER, true, false );
    sourceData.widthHint = 200;
    sourceText.setLayoutData( sourceData );
    sourceText.setText( theme.getSource() );
    sourceText.addModifyListener( new ModifyListener()
    {
      public void modifyText( final ModifyEvent e )
      {
        final String source = sourceText.getText();
        setThemeSource( source );
      }
    } );

    return composite;
  }


  /**
   * This function returns the theme.
   * 
   * @return The theme.
   */
  private KalypsoWMSTheme getTheme( )
  {
    final IAdaptable element = getElement();
    final KalypsoWMSTheme theme = (KalypsoWMSTheme) (element instanceof KalypsoWMSTheme ? element : element.getAdapter( KalypsoWMSTheme.class ));
    return theme;
  }

  /**
   * @see org.eclipse.jface.preference.PreferencePage#performOk()
   */
  @Override
  public boolean performOk( )
  {
    final IKalypsoTheme theme = getTheme();
    if( theme == null )
      return false;

    final CompositeCommand allCommands = new CompositeCommand( Messages.getString( "org.kalypso.ui.views.properties.ThemePropertyPage.1" ) ); //$NON-NLS-1$

    // TODO: we do not know yet how to change the source

    // TODO: find mapPanel!
    try
    {
      allCommands.process();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    return super.performOk();
  }

  private void checkValid( )
  {
    /* clear error */
    setErrorMessage( null );

    /* Revalidate and set message accoring to result */
    final String msg = validate();
    if( msg == null )
      setValid( true );
    else if( msg.length() == 0 )
      setValid( false );
    else
    {
      setValid( false );
      setErrorMessage( msg );
    }
  }

  private String validate( )
  {
    // If nothing changed, not valid, but also no message
    if( m_themeSource == null )
      return "";

    // TODO: check validity of source-URL; reuse code of add WMS theme functionality (refaktor!)

    // everything else is valid
    return null;
  }

  /**
   * This function sets the name of the theme.
   * 
   * @param name
   *            The name of the theme.
   */
  protected void setThemeSource( final String source )
  {
    m_themeSource = source;

    checkValid();
  }
}
