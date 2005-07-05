/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wbprivate final ExportMapOptionsPage 

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
package org.kalypso.ogc.gml.map.wizard;

import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.io.OutputStream;

import javax.imageio.ImageIO;

import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.contribs.eclipse.swt.events.DoubleModifyListener;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.MapModellHelper;
import org.kalypso.ui.metadoc.IExportableDocument;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * @author belger
 */
public class ExportMapOptionsPage extends WizardPage implements IExportableDocument
{
  private final static String STORE_WIDTH_ID = "ExportMapWizardPage.STORE_WIDTH_ID"; //$NON-NLS-1$

  private final static String STORE_HEIGHT_ID = "ExportMapWizardPage.STORE_HEIGHT_ID"; //$NON-NLS-1$

  protected static final int SIZING_TEXT_FIELD_WIDTH = 100;

  private Text m_widthtext;

  private Text m_heighttext;

  private final int m_width;

  private final int m_height;

  private final MapPanel m_panel;

  private boolean m_result;

  private String m_destinationFormat;

  public ExportMapOptionsPage( final MapPanel panel, final String pageName, final String title,
      final ImageDescriptor titleImage, final int width, final int height )
  {
    super( pageName, title, titleImage );
    m_panel = panel;

    m_width = width;
    m_height = height;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    panel.setLayout( new GridLayout() );

    panel.setLayoutData( new GridData( GridData.VERTICAL_ALIGN_FILL | GridData.HORIZONTAL_ALIGN_FILL ) );
    panel.setFont( parent.getFont() );

    createExportOptionsGroup( panel );

    setControl( panel );

    restoreWidgetValues();
  }

  private void createExportOptionsGroup( final Composite parent )
  {
    final Font font = parent.getFont();

    final Group optionsGroup = new Group( parent, SWT.NONE );
    final GridLayout layout = new GridLayout( 2, false );
    optionsGroup.setLayout( layout );
    optionsGroup.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    optionsGroup.setText( "Export Optionen" );
    optionsGroup.setFont( font );

    final Color red = parent.getDisplay().getSystemColor( SWT.COLOR_RED );
    final Color black = parent.getDisplay().getSystemColor( SWT.COLOR_BLACK );
    final DoubleModifyListener doubleModifyListener = new DoubleModifyListener( black, red );

    final ModifyListener updateModifyListener = new ModifyListener()
    {
      public void modifyText( ModifyEvent e )
      {
        updatePageCompletion();
      }
    };

    final Label widthlabel = new Label( optionsGroup, SWT.NONE );
    widthlabel.setText( "Breite:" );

    m_widthtext = new Text( optionsGroup, SWT.BORDER );
    m_widthtext.setText( "" + m_width );
    m_widthtext.setFont( font );
    m_widthtext.addModifyListener( doubleModifyListener );
    m_widthtext.addModifyListener( updateModifyListener );

    final GridData widthdata = new GridData( GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL );
    widthdata.widthHint = SIZING_TEXT_FIELD_WIDTH;
    m_widthtext.setLayoutData( widthdata );

    final Label heightlabel = new Label( optionsGroup, SWT.NONE );
    heightlabel.setText( "Höhe:" );

    m_heighttext = new Text( optionsGroup, SWT.BORDER );
    m_heighttext.setText( "" + m_height );
    m_heighttext.setFont( font );
    m_heighttext.addModifyListener( doubleModifyListener );
    m_heighttext.addModifyListener( updateModifyListener );

    final GridData heightdata = new GridData( GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL );
    heightdata.widthHint = SIZING_TEXT_FIELD_WIDTH;
    m_heighttext.setLayoutData( heightdata );
  }

  public int getWidth()
  {
    try
    {
      return Integer.parseInt( m_widthtext.getText() );
    }
    catch( NumberFormatException e )
    {
      return -1;
    }
  }

  public int getHeight()
  {
    try
    {
      return Integer.parseInt( m_heighttext.getText() );
    }
    catch( final NumberFormatException e )
    {
      return -1;
    }
  }

  /**
   * Hook method for restoring widget values to the values that they held last time this wizard was used to completion.
   */
  protected void restoreWidgetValues()
  {
    final IDialogSettings settings = getDialogSettings();
    if( settings != null )
    {
      try
      {
        // Lieber die aktuelle Kartengrösse als die gespeicherten Werte
        //        m_widthtext.setText( "" + settings.getInt( STORE_WIDTH_ID ) );
        //        m_heighttext.setText( "" + settings.getInt( STORE_HEIGHT_ID ) );
      }
      catch( final Exception e )
      {
        // ignore: noch keine Einstellungen da
      }
    }
  }

  public void saveWidgetValues()
  {
    final IDialogSettings settings = getDialogSettings();
    if( settings != null )
    {
      settings.put( STORE_WIDTH_ID, getWidth() );
      settings.put( STORE_HEIGHT_ID, getHeight() );
    }
  }

  protected void updatePageCompletion()
  {
    boolean pageComplete = determinePageCompletion();

    setPageComplete( pageComplete );
    if( pageComplete )
    {
      setErrorMessage( null );
      setMessage( null );
    }
  }

  private boolean determinePageCompletion()
  {
    final int width = getWidth();
    final int height = getHeight();

    if( width == -1 || height == -1 )
    {
      setMessage( "Geben Sie Höhe und Breite als ganze Zahlen ein." );
      setErrorMessage( width == -1 ? "Breite ist keine ganze Zahl" : "Höhe ist keine ganze Zahl" );
      return false;
    }

    return true;
  }

  /**
   * @throws IOException
   * @see org.kalypso.ui.metadoc.IExportableDocument#exportDocument(java.io.OutputStream)
   */
  public void exportDocument( final OutputStream outs ) throws IOException
  {
    final IMapModell mapModell = m_panel.getMapModell();

    final GeoTransform transform = m_panel.getProjection();
    final GM_Envelope boundingBox = m_panel.getBoundingBox();

    final Rectangle bounds = new Rectangle( m_width, m_height );
    final BufferedImage image = MapModellHelper.createImageFromModell( transform, boundingBox, bounds, bounds.width,
        bounds.height, mapModell, -1 );

    m_result = ImageIO.write( image, m_destinationFormat, outs );
  }

  public void setDestinationFormat( final String destinationFormat )
  {
    m_destinationFormat = destinationFormat;
  }

  /**
   * @see org.eclipse.jface.wizard.WizardPage#setDescription(java.lang.String)
   */
  public void setDescription( String description )
  {
    super.setDescription( description );
  }

  /**
   * @see org.kalypso.ui.metadoc.IExportableDocument#getDocumentExtension()
   */
  public String getDocumentExtension()
  {
    return ".csv";
  }

  public boolean isResult()
  {
    return m_result;
  }

}