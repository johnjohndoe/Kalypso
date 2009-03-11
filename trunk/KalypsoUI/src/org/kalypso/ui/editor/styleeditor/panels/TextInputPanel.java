/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.editor.styleeditor.panels;

import java.util.Locale;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.ogc.gml.IKalypsoUserStyleListener;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.editor.mapeditor.GisMapEditor;
import org.kalypso.ui.editor.styleeditor.MessageBundle;

/**
 * @author F.Lindemann
 */
public class TextInputPanel
{
  private final Composite m_parent;

  private final FormToolkit m_toolkit;

  private final int m_parentColumns;

  public static interface ModifyListener
  {
    String textModified( final String newValue );
  }

  public TextInputPanel( final FormToolkit toolkit, final Composite parent )
  {
    m_toolkit = toolkit;
    m_parent = parent;
    final GridLayout layout = (GridLayout) parent.getLayout();
    m_parentColumns = layout.numColumns;
  }

  public void createTextRow( final String label, final String text, final ModifyListener listener )
  {
    final Label urlLabel = m_toolkit.createLabel( m_parent, label );
    urlLabel.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, false, false ) );

    final Text textControl = m_toolkit.createText( m_parent, text );
    final GridData textData = new GridData( SWT.FILL, SWT.CENTER, true, false );
    textData.horizontalSpan = m_parentColumns - 1;
    textControl.setLayoutData( textData );

    configureListeners( listener, textControl );
  }

  public void createDenominatorRow( final String label, final double value, final ModifyListener listener )
  {
    final Label labelControl = m_toolkit.createLabel( m_parent, label );
    labelControl.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, false, false ) );

    final Text textControl = m_toolkit.createText( m_parent, "" + value ); //$NON-NLS-1$
    final GridData textData = new GridData( SWT.FILL, SWT.CENTER, true, false );
    textData.horizontalSpan = m_parentColumns - 2;
    textControl.setLayoutData( textData );

    final Button getCurrentScaleButton = m_toolkit.createButton( m_parent, "", SWT.PUSH | SWT.FLAT );
    getCurrentScaleButton.setLayoutData( new GridData( SWT.CENTER, SWT.CENTER, false, false ) );
    getCurrentScaleButton.setImage( ImageProvider.IMAGE_STYLEEDITOR_GET_SCALE.createImage() );
    getCurrentScaleButton.setToolTipText( MessageBundle.STYLE_EDITOR_SCALE );
    getCurrentScaleButton.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
        final IEditorPart editor = window.getActivePage().getActiveEditor();
        if( editor instanceof GisMapEditor )
        {
          // TODO: get from current context instead!
          final GisMapEditor gisMapEditor = (GisMapEditor) editor;
          final double currentScale = gisMapEditor.getMapPanel().getCurrentScale();
          final String newText = String.format( Locale.PRC, "%.2f", currentScale );//$NON-NLS-1$
          textControl.setText( newText );
          final String reset = listener.textModified( newText );
          if( reset != null )
            textControl.setText( reset );
        }
      }
    } );

    configureListeners( listener, textControl );
  }

// MessageBundle.STYLE_EDITOR_LEGEND
  void createLegendRow( final String label, final KalypsoUserStyle userStyle )
  {
    final Label labelControl = m_toolkit.createLabel( m_parent, label, SWT.NULL );
    labelControl.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, false, false ) );

    final Label legendLabel = m_toolkit.createLabel( m_parent, null );
    labelControl.setLayoutData( new GridData( SWT.CENTER, SWT.CENTER, false, false ) );

    final LegendLabel legendControl = new LegendLabel( legendLabel, userStyle, 0 );

    final IKalypsoUserStyleListener styleListener = new IKalypsoUserStyleListener()
    {
      @Override
      public void styleChanged( final KalypsoUserStyle source )
      {
        // TODO Auto-generated method stub
        legendControl.updateLegendImage();
      }
    };
    userStyle.addStyleListener( styleListener );

    labelControl.addDisposeListener( new DisposeListener()
    {
      @Override
      public void widgetDisposed( final DisposeEvent e )
      {
        userStyle.removeStyleListener( styleListener );
      }
    } );
  }

  private void configureListeners( final ModifyListener listener, final Text textControl )
  {
    textControl.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetDefaultSelected( final SelectionEvent e )
      {
        final String reset = listener.textModified( textControl.getText() );
        if( reset != null )
          textControl.setText( reset );
      }
    } );

    textControl.addFocusListener( new FocusAdapter()
    {
      /**
       * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
       */
      @Override
      public void focusLost( final FocusEvent e )
      {
        final String reset = listener.textModified( textControl.getText() );
        if( reset != null )
          textControl.setText( reset );
      }
    } );

// textControl.addModifyListener( new org.eclipse.swt.events.ModifyListener()
// {
// @Override
// public void modifyText( final ModifyEvent e )
// {
// listener.textModified( textControl.getText() );
// }
// } );
  }

}