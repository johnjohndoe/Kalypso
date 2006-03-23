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
package org.kalypso.metadoc.ui;

import org.apache.commons.configuration.Configuration;
import org.apache.commons.lang.ArrayUtils;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.metadoc.configuration.IConfigurationListener;
import org.kalypso.metadoc.configuration.IPublishingConfiguration;
import org.kalypso.metadoc.impl.FileExportTarget;

/**
 * @author belger
 */
public class ImageExportPage extends WizardPage implements IConfigurationListener
{
  public final static String CONF_IMAGE_FORMAT = ImageExportPage.class.getName() + ".IMAGE_FORMAT"; //$NON-NLS-1$

  public final static String CONF_IMAGE_WIDTH = ImageExportPage.class.getName() + ".IMAGE_WIDTH"; //$NON-NLS-1$

  public final static String CONF_IMAGE_HEIGHT = ImageExportPage.class.getName() + ".IMAGE_HEIGHT"; //$NON-NLS-1$

  private static final String CONF_IMAGE_RATIO = ImageExportPage.class.getName() + ".IMAGE_RATIO"; //$NON-NLS-1$

  private final static String STORE_FORMAT_ID = CONF_IMAGE_FORMAT;

  private final static String STORE_WIDTH_ID = CONF_IMAGE_WIDTH;

  private final static String STORE_HEIGHT_ID = CONF_IMAGE_HEIGHT;

  private static final String STORE_IMAGE_RATIO_ID = CONF_IMAGE_RATIO;

  protected static final int SIZING_TEXT_FIELD_WIDTH = 100;

  private final static String[] FORMATS = { "gif", "jpeg", "png" };

  private final static String[] EXTENSIONS = { ".gif", ".jpg", ".png" };

  private final IPublishingConfiguration m_conf;

  private Text m_widthtext;

  private Text m_heighttext;

  private final double m_heigthToWithImageRatio;

  private ComboViewer m_combo;

  private ISelectionChangedListener m_formatSelListener = new ISelectionChangedListener()
  {
    public void selectionChanged( final SelectionChangedEvent event )
    {
      updateConf();
    }
  };

  private SelectionListener m_selectionListener = new SelectionAdapter()
  {

    @Override
    public void widgetSelected( SelectionEvent e )
    {
      updateConf();
    }
  };

  private Button m_keepImageRatio;

  public ImageExportPage( final IPublishingConfiguration conf, final String pageName, final String title, final ImageDescriptor titleImage, double hintImageRatio )
  {
    super( pageName, title, titleImage );

    m_conf = conf;
    m_conf.addListener( this ); // removed in dispose()
    m_heigthToWithImageRatio = hintImageRatio;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#dispose()
   */
  @Override
  public void dispose( )
  {
    m_conf.removeListener( this );

    if( m_combo != null )
      m_combo.removeSelectionChangedListener( m_formatSelListener );
    if( m_keepImageRatio != null )
      m_keepImageRatio.removeSelectionListener( m_selectionListener );

    super.dispose();
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

    final FocusListener updateModifyListener = new FocusListener()
    {
      public void focusLost( FocusEvent e )
      {
        updateConf();
      }

      public void focusGained( FocusEvent e )
      {
        // only update when focus is lost

      }
    };

    final Label widthlabel = new Label( optionsGroup, SWT.NONE );
    widthlabel.setText( "Breite [Pixel]:" );

    m_widthtext = new Text( optionsGroup, SWT.BORDER );
    m_widthtext.setFont( font );
    m_widthtext.addFocusListener( updateModifyListener );

    final GridData widthdata = new GridData( GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL );
    widthdata.widthHint = SIZING_TEXT_FIELD_WIDTH;
    m_widthtext.setLayoutData( widthdata );

    final Label heightlabel = new Label( optionsGroup, SWT.NONE );
    heightlabel.setText( "Höhe [Pixel]:" );

    m_heighttext = new Text( optionsGroup, SWT.BORDER );
    m_heighttext.setFont( font );
    m_heighttext.addFocusListener( updateModifyListener );

    final GridData heightdata = new GridData( GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL );
    heightdata.widthHint = SIZING_TEXT_FIELD_WIDTH;
    m_heighttext.setLayoutData( heightdata );

    final Label comboLabel = new Label( optionsGroup, SWT.NONE );
    comboLabel.setText( "Format:" );

    m_combo = new ComboViewer( optionsGroup, SWT.BORDER | SWT.READ_ONLY );
    m_combo.setLabelProvider( new LabelProvider() );
    m_combo.setContentProvider( new ArrayContentProvider() );
    m_combo.setInput( FORMATS );
    m_combo.addSelectionChangedListener( m_formatSelListener );
    // m_combo.setSelection( new StructuredSelection( FORMATS[0] ) );

    final GridData combodata = new GridData( GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL );
    combodata.widthHint = SIZING_TEXT_FIELD_WIDTH;
    m_combo.getControl().setLayoutData( combodata );

    final Label ratioLabel = new Label( optionsGroup, SWT.NONE );
    ratioLabel.setText( "Verhältnis beibehalten:" );

    if( m_heigthToWithImageRatio > 0 )
    {
      m_keepImageRatio = new Button( optionsGroup, SWT.CHECK );
      m_keepImageRatio.addSelectionListener( m_selectionListener );
      m_keepImageRatio.setToolTipText( "Fixiert das Breiten- zu Höhenverhältnis aus der aktiven Karte,relative zur Breite" );
    }
  }

  protected void updateConf( )
  {
    String msg = "";

    final Integer width = NumberUtils.parseQuietInteger( m_widthtext.getText() );
    if( width == null || width.intValue() <= 0 )
      msg += "Ungültige Breite";
    else
      m_conf.setProperty( CONF_IMAGE_WIDTH, width );

    final Integer height = NumberUtils.parseQuietInteger( m_heighttext.getText() );
    if( height == null || height.intValue() <= 0 )
      msg += (msg.length() != 0 ? "\n " : "") + "Ungültige Höhe";
    else
      m_conf.setProperty( CONF_IMAGE_HEIGHT, height );

    final String format = (String) ((IStructuredSelection) m_combo.getSelection()).getFirstElement();
    m_conf.setProperty( CONF_IMAGE_FORMAT, format );
    final int indexOf = ArrayUtils.indexOf( FORMATS, format );
    if( indexOf != -1 )
      m_conf.setProperty( FileExportTarget.CONF_FILEEXPORT_EXTENSION, EXTENSIONS[indexOf] );

    boolean selection = m_keepImageRatio.getSelection();
    m_conf.setProperty( CONF_IMAGE_RATIO, selection );
    if( selection )
      msg = "";
    setErrorMessage( msg.length() == 0 ? null : msg );
    setPageComplete( msg.length() == 0 );

    saveWidgetValues();
  }

  private void updateControls( final String key )
  {
    if( key.equals( CONF_IMAGE_WIDTH ) || key == null )
    {
      final String width = "" + m_conf.getInt( CONF_IMAGE_WIDTH, 300 );
      if( !m_widthtext.getText().equals( width ) )
        m_widthtext.setText( width );
    }

    if( key.equals( CONF_IMAGE_HEIGHT ) || key == null )
    {
      final String height = "" + m_conf.getInt( CONF_IMAGE_HEIGHT, 300 );
      if( !m_heighttext.getText().equals( height ) )
        m_heighttext.setText( height );
    }

    if( key.equals( CONF_IMAGE_FORMAT ) || key == null )
    {
      final String format = m_conf.getString( CONF_IMAGE_FORMAT, FORMATS[0] );

      final String oldFormat = (String) ((IStructuredSelection) m_combo.getSelection()).getFirstElement();
      if( !format.equals( oldFormat ) )
        m_combo.setSelection( new StructuredSelection( format ) );
    }
    if( key.equals( CONF_IMAGE_RATIO ) || key == null )
    {
      boolean selection = m_conf.getBoolean( CONF_IMAGE_RATIO );
      if( selection && m_keepImageRatio != null )
      {
        Integer width = NumberUtils.parseQuietInteger( m_widthtext.getText() );
        m_heighttext.setText( String.valueOf( (int) (width.intValue() / m_heigthToWithImageRatio) ) );
      }
    }
  }

  /**
   * Hook method for restoring widget values to the values that they held last time this wizard was used to completion.
   */
  protected void restoreWidgetValues( )
  {
    final IDialogSettings settings = getDialogSettings();
    if( settings != null )
    {
      try
      {
        // get all values first, because the 'set' will change the settings
        final int width = settings.getInt( STORE_WIDTH_ID );
        final int height = settings.getInt( STORE_HEIGHT_ID );
        final String format = settings.get( STORE_FORMAT_ID );
        final boolean ratio = settings.getBoolean( STORE_IMAGE_RATIO_ID );

        m_widthtext.setText( "" + width );
        m_heighttext.setText( "" + height );
        m_combo.setSelection( new StructuredSelection( format ) );
        m_keepImageRatio.setSelection( ratio );
      }
      catch( final Exception e )
      {
        // ignore: noch keine Einstellungen da
      }
    }
  }

  public void saveWidgetValues( )
  {
    final IDialogSettings settings = getDialogSettings();
    if( settings != null )
    {
      settings.put( STORE_WIDTH_ID, m_conf.getInt( CONF_IMAGE_WIDTH, 0 ) );
      settings.put( STORE_HEIGHT_ID, m_conf.getInt( CONF_IMAGE_HEIGHT, 0 ) );
      settings.put( STORE_FORMAT_ID, m_conf.getString( CONF_IMAGE_FORMAT, FORMATS[0] ) );
      settings.put( STORE_IMAGE_RATIO_ID, m_conf.getBoolean( CONF_IMAGE_RATIO, true ) );
    }
  }

  /**
   * @see org.kalypso.metadoc.configuration.IConfigurationListener#configurationChanged(org.apache.commons.configuration.Configuration,
   *      java.lang.String)
   */
  public void configurationChanged( final Configuration config, final String key )
  {
    final Control control = getControl();
    if( control == null )
      return;
    final Display display = control.getDisplay();
    if( display.isDisposed() )
      return;

    display.asyncExec( new Runnable()
    {
      public void run( )
      {
        updateControls( key );
      }
    } );
  }
}