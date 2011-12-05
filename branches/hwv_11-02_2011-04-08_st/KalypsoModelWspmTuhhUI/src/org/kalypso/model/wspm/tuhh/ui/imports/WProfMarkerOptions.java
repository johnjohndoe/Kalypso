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

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.contribs.eclipse.ui.forms.MessageProvider;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class WProfMarkerOptions
{
  private final Map<String, int[]> m_markerMap = new HashMap<String, int[]>();

  private final WProfOptionsPage m_page;

  private IMessageProvider m_currentMessage;

  public WProfMarkerOptions( final WProfOptionsPage page )
  {
    m_page = page;
  }

  public Control createControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    panel.setLayout( new GridLayout( 2, false ) );
    final Label label = new Label( panel, SWT.NONE );
    label.setText( Messages.getString("WProfMarkerOptions_0") ); //$NON-NLS-1$
    label.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );

    createMarkerRow( panel, Messages.getString("WProfMarkerOptions_1"), IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE, false ); //$NON-NLS-1$
    createMarkerRow( panel, Messages.getString("WProfMarkerOptions_2"), IWspmTuhhConstants.MARKER_TYP_BORDVOLL, true ); //$NON-NLS-1$
    createMarkerRow( panel, Messages.getString("WProfMarkerOptions_3"), IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE, true ); //$NON-NLS-1$

    return panel;
  }

  public Map<String, int[]> getMarkerMappings( )
  {
    return Collections.unmodifiableMap( m_markerMap );
  }

  private void createMarkerRow( final Composite parent, final String labelText, final String markerType, final boolean isOptional )
  {
    final Label label = new Label( parent, SWT.NONE );
    label.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, false, false ) );
    label.setText( labelText );

    final Text text = new Text( parent, SWT.BEGINNING | SWT.BORDER );
    text.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final IDialogSettings dialogSettings = m_page.getWizard().getDialogSettings();
    if( dialogSettings != null )
    {
      final String lastText = dialogSettings.get( markerType );
      if( lastText != null )
      {
        text.setText( lastText );
        handleTextChanged( labelText, lastText, markerType, isOptional );
      }
    }

// if( m_markerMap.containsKey( markerType ) )
// {
// final int[] values = m_markerMap.get( markerType );
// if( values != null )
// text.setText( Arrays.dump( values ) );
// }

    text.addModifyListener( new ModifyListener()
    {
      @Override
      public void modifyText( final ModifyEvent e )
      {
        final String currentText = text.getText();
        handleTextChanged( labelText, currentText, markerType, isOptional );
      }
    } );
  }

  protected void handleTextChanged( final String label, final String currentText, final String markerType, final boolean isOptional )
  {
    m_currentMessage = doSetText( label, currentText, markerType, isOptional );

    m_page.validate();
  }

  private IMessageProvider doSetText( final String label, final String currentText, final String markerType, final boolean isOptional )
  {
    try
    {
      final int[] values = parseIntList( currentText );

      /* Set dialog settings, if parsing was successful */
      final IDialogSettings dialogSettings = m_page.getWizard().getDialogSettings();
      if( dialogSettings != null )
        dialogSettings.put( markerType, currentText );

      if( values.length == 0 )
      {
        if( !isOptional )
          return new MessageProvider( String.format( Messages.getString("WProfMarkerOptions_4"), label ), IMessageProvider.ERROR ); //$NON-NLS-1$

        return null;
      }

      m_markerMap.put( markerType, values );
      return null;
    }
    catch( final NumberFormatException e )
    {
      return new MessageProvider( String.format( Messages.getString("WProfMarkerOptions_5"), currentText, e.getLocalizedMessage() ), IMessageProvider.ERROR ); //$NON-NLS-1$
    }
  }

  private int[] parseIntList( final String currentText ) throws NumberFormatException
  {
    String[] split = currentText.split( ",", -1 ); //$NON-NLS-1$

    while( true )
    {
      final int splitLenght = split.length;
      split = (String[]) ArrayUtils.removeElement( split, "" ); //$NON-NLS-1$
      if( splitLenght == split.length )
        break;
    }

    final int[] ids = new int[split.length];
    for( int i = 0; i < ids.length; i++ )
      ids[i] = Integer.parseInt( split[i].trim() );

    return ids;
  }

  public IMessageProvider validate( )
  {
    return m_currentMessage;
  }
}
