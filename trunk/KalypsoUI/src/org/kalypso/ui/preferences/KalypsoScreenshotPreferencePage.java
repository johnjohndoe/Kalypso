/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.ui.preferences;

import java.io.File;
import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.preference.PreferenceStore;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

/**
 * @author kuch
 */
public class KalypsoScreenshotPreferencePage extends PreferencePage implements IWorkbenchPreferencePage
{
  private static final String KALYPSO_MAP_SCREENSHOT_SETTINGS = "kalypso.map.screenshot.settings";

  private static final String DEFAULT_SCREENSHOT_WIDTH = "400";

  public static final String KEY_SCREENSHOT_WIDTH = "kalypso_screenshot_width";

  public static final String KEY_SCREENSHOT_HEIGHT = "kalypso_screenshot_height";

  public static final String KEY_SCREENSHOT_FORMAT = "kalypso_screenshot_format";

  public static final String KEY_SCREENSHOT_TARGET = "kalypso_screenshot_target";

  private static final String DEFAULT_SCREENSHOT_HEIGHT = "300";

  private static final String DEFAULT_SCREENSHOT_FORMAT = "png";

  private static final String DEFAULT_SCREENSHOT_TARGET = "c:\\temp";

  static public IPreferenceStore getPreferences( )
  {
    try
    {
      final PreferenceStore mystore = new PreferenceStore( KalypsoScreenshotPreferencePage.KALYPSO_MAP_SCREENSHOT_SETTINGS );
      mystore.load();

      KalypsoScreenshotPreferencePage.checkStore( mystore );

      return mystore;
    }
    catch( final IOException e )
    {
      try
      {
        final PreferenceStore mystore = new PreferenceStore( KalypsoScreenshotPreferencePage.KALYPSO_MAP_SCREENSHOT_SETTINGS );

        mystore.setValue( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_WIDTH, KalypsoScreenshotPreferencePage.DEFAULT_SCREENSHOT_WIDTH );
        mystore.setValue( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_HEIGHT, KalypsoScreenshotPreferencePage.DEFAULT_SCREENSHOT_HEIGHT );
        mystore.setValue( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_FORMAT, KalypsoScreenshotPreferencePage.DEFAULT_SCREENSHOT_FORMAT );

        mystore.save();

        return mystore;
      }
      catch( final IOException e1 )
      {
        e1.printStackTrace();
      }
    }

    throw new IllegalStateException();
  }

  private ComboViewer m_cmbFormat;

  private Text m_height;

  private Text m_width;

  protected Text m_target;

  public KalypsoScreenshotPreferencePage( )
  {
    super();

    setTitle( "Kalypso map screenshot settings" );
    setDescription( "Configure your basic screenshot settings." );
  }

  protected void checkPage( )
  {
    setValid( false );

    final String sHeight = m_height.getText();

    Pattern p = Pattern.compile( "\\d*\\D+\\d*" );
    Matcher m = p.matcher( sHeight );
    if( (sHeight == null) || m.matches() )
    {
      setMessage( "" );
      setErrorMessage( "Invalid screenshot height." );

      return;
    }

    final int iHeight = new Integer( sHeight );
    if( iHeight <= 0 )
    {
      setMessage( "" );
      setErrorMessage( "Invalid screenshot height." );

      return;
    }

    final String sWidth = m_width.getText();

    p = Pattern.compile( "\\d*\\D+\\d*" );
    m = p.matcher( sWidth );
    if( (sWidth == null) || m.matches() )
    {
      setMessage( "" );
      setErrorMessage( "Invalid screenshot width." );

      return;
    }

    final int iWidth = new Integer( sWidth );

    if( iWidth <= 0 )
    {
      setMessage( "" );
      setErrorMessage( "Invalid screenshot width." );

      return;
    }

    final ISelection selection = m_cmbFormat.getSelection();
    if( selection == null )
    {
      setMessage( "" );
      setErrorMessage( "Invalid screenshot export format." );

      return;
    }

    final String text = m_target.getText();
    if( (text == null) || "".equals( text ) )
    {
      setMessage( "" );
      setErrorMessage( "Target directory missing." );

      return;
    }

    final File file = new File( text );
    if( !file.exists() )
    {
      setMessage( "" );
      setErrorMessage( "Target directory doesn't exists. Select an existing dir." );

      return;
    }

    if( !file.isDirectory() )
    {
      setMessage( "" );
      setErrorMessage( "Target directory isn't a directory." );

      return;
    }

    setMessage( null );
    setErrorMessage( null );

    setValid( true );
  }

  /**
   * @see org.eclipse.jface.preference.PreferencePage#createContents(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createContents( final Composite parent )
  {

    final Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayout( new GridLayout() );

    /*******************************************************************************************************************
     * SCREENSHOTS
     ******************************************************************************************************************/
    final Group grScreen = new Group( composite, SWT.NONE );
    grScreen.setText( "Format settings" );
    grScreen.setLayout( new GridLayout() );
    grScreen.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    final Composite cgrScreen = new Composite( grScreen, SWT.NONE );
    cgrScreen.setLayout( new GridLayout( 2, false ) );
    cgrScreen.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    /* screenshot width */
    final Label lWidth = new Label( cgrScreen, SWT.NONE );
    lWidth.setText( "Width" );

    m_width = new Text( cgrScreen, SWT.BORDER );
    m_width.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    final String width = getPreferenceStore().getString( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_WIDTH );
    if( width == null )
      m_width.setText( getPreferenceStore().getDefaultString( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_WIDTH ) );
    else
      m_width.setText( width );

    m_width.addModifyListener( new ModifyListener()
    {
      public void modifyText( final ModifyEvent e )
      {
        checkPage();
      }
    } );

    /* screenshot height */
    final Label lHeight = new Label( cgrScreen, SWT.NONE );
    lHeight.setText( "Height" );

    m_height = new Text( cgrScreen, SWT.BORDER );
    m_height.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    final String height = getPreferenceStore().getString( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_HEIGHT );
    if( height == null )
      m_height.setText( getPreferenceStore().getDefaultString( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_HEIGHT ) );
    else
      m_height.setText( height );

    m_height.addModifyListener( new ModifyListener()
    {
      public void modifyText( final ModifyEvent e )
      {
        checkPage();
      }
    } );

    /* screenshot format */
    final Label lFormat = new Label( cgrScreen, SWT.NONE );
    lFormat.setText( "Format" );

    final String[] formats = new String[] { "jpg", "png" };

    m_cmbFormat = new ComboViewer( cgrScreen, SWT.BORDER | SWT.READ_ONLY | SWT.SINGLE );
    m_cmbFormat.setContentProvider( new ArrayContentProvider() );
    m_cmbFormat.setLabelProvider( new LabelProvider()
    {
      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @Override
      public String getText( final Object element )
      {
        if( element instanceof String )
        {
          final String value = (String) element;
          if( "jpg".equals( value ) )
            return "JPEG";
          else if( "png".equals( value ) )
            return "PNG";
        }

        return super.getText( element );
      }
    } );
    m_cmbFormat.setInput( formats );
    m_cmbFormat.getCombo().setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    final String format = getPreferenceStore().getString( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_FORMAT );
    if( m_height == null )
      m_cmbFormat.setSelection( new StructuredSelection( getPreferenceStore().getDefaultString( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_FORMAT ) ) );
    else
      m_cmbFormat.setSelection( new StructuredSelection( format ) );

    m_cmbFormat.addSelectionChangedListener( new ISelectionChangedListener()
    {

      public void selectionChanged( final SelectionChangedEvent event )
      {
        checkPage();
      }
    } );

    /* target folder */
    final Group grTarget = new Group( composite, SWT.NONE );
    grTarget.setText( "Storage settings" );
    grTarget.setLayout( new GridLayout( 3, false ) );
    grTarget.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    final Label lTarget = new Label( grTarget, SWT.NONE );
    lTarget.setText( "Store into folder:" );

    m_target = new Text( grTarget, SWT.READ_ONLY | SWT.BORDER );
    m_target.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    m_target.addModifyListener( new ModifyListener()
    {
      public void modifyText( final ModifyEvent e )
      {
        checkPage();
      }
    } );

    final Button bTarget = new Button( grTarget, SWT.NONE );
    bTarget.setText( "..." );

    bTarget.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final DirectoryDialog dialog = new DirectoryDialog( bTarget.getShell() );
        m_target.setText( dialog.open() );
      }
    } );

    final String targetDir = getPreferenceStore().getString( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_TARGET );
    if( targetDir == null )
      m_height.setText( getPreferenceStore().getDefaultString( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_TARGET ) );
    else
      m_target.setText( targetDir );

    checkPage();

    return composite;
  }

  /**
   * @see org.eclipse.jface.preference.PreferencePage#getPreferenceStore()
   */
  @Override
  public IPreferenceStore getPreferenceStore( )
  {
    IPreferenceStore store = super.getPreferenceStore();

    if( store == null )
    {
      final PreferenceStore mystore = new PreferenceStore( KalypsoScreenshotPreferencePage.KALYPSO_MAP_SCREENSHOT_SETTINGS );
      try
      {
        mystore.load();
      }
      catch( final IOException e )
      {
        e.printStackTrace();
      }

      store = mystore;
      setPreferenceStore( store );
    }

    KalypsoScreenshotPreferencePage.checkStore( store );

    return store;
  }

  static private void checkStore( final IPreferenceStore store )
  {
    final String width = store.getString( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_WIDTH );
    if( (width == null) || "".equals( width ) )
      store.setValue( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_WIDTH, KalypsoScreenshotPreferencePage.DEFAULT_SCREENSHOT_HEIGHT );

    final String height = store.getString( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_HEIGHT );
    if( (height == null) || "".equals( height ) )
      store.setValue( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_HEIGHT, KalypsoScreenshotPreferencePage.DEFAULT_SCREENSHOT_HEIGHT );

    final String format = store.getString( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_FORMAT );
    if( (format == null) || "".equals( format ) )
      store.setValue( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_FORMAT, KalypsoScreenshotPreferencePage.DEFAULT_SCREENSHOT_FORMAT );

    final String target = store.getString( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_TARGET );
    if( (target == null) || "".equals( target ) )
      store.setValue( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_TARGET, KalypsoScreenshotPreferencePage.DEFAULT_SCREENSHOT_TARGET );

    if( store instanceof PreferenceStore )
    {
      final PreferenceStore pStrore = (PreferenceStore) store;
      try
      {
        pStrore.save();
      }
      catch( final IOException e )
      {
        e.printStackTrace();
      }
    }
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
   */
  public void init( final IWorkbench workbench )
  {
    getPreferenceStore().setDefault( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_WIDTH, KalypsoScreenshotPreferencePage.DEFAULT_SCREENSHOT_WIDTH );
    getPreferenceStore().setDefault( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_HEIGHT, KalypsoScreenshotPreferencePage.DEFAULT_SCREENSHOT_HEIGHT );
    getPreferenceStore().setDefault( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_FORMAT, KalypsoScreenshotPreferencePage.DEFAULT_SCREENSHOT_FORMAT );
    getPreferenceStore().setDefault( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_TARGET, KalypsoScreenshotPreferencePage.DEFAULT_SCREENSHOT_TARGET );
  }

  /**
   * @see org.eclipse.jface.preference.PreferencePage#performDefaults()
   */
  @Override
  protected void performDefaults( )
  {
    getPreferenceStore().setValue( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_WIDTH, KalypsoScreenshotPreferencePage.DEFAULT_SCREENSHOT_WIDTH );
    getPreferenceStore().setValue( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_HEIGHT, KalypsoScreenshotPreferencePage.DEFAULT_SCREENSHOT_HEIGHT );
    getPreferenceStore().setValue( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_FORMAT, KalypsoScreenshotPreferencePage.DEFAULT_SCREENSHOT_FORMAT );
    getPreferenceStore().setValue( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_TARGET, KalypsoScreenshotPreferencePage.DEFAULT_SCREENSHOT_TARGET );

    m_width.setText( getPreferenceStore().getString( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_WIDTH ) );
    m_height.setText( getPreferenceStore().getString( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_HEIGHT ) );
    m_cmbFormat.setSelection( new StructuredSelection( getPreferenceStore().getString( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_FORMAT ) ) );
    m_cmbFormat.setSelection( new StructuredSelection( getPreferenceStore().getString( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_TARGET ) ) );
    super.performDefaults();
  }

  /**
   * @see org.eclipse.jface.preference.PreferencePage#performOk()
   */
  @Override
  public boolean performOk( )
  {
    getPreferenceStore().setValue( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_WIDTH, m_width.getText() );
    getPreferenceStore().setValue( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_HEIGHT, m_height.getText() );
    getPreferenceStore().setValue( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_TARGET, m_target.getText() );

    final ISelection selection = m_cmbFormat.getSelection();
    if( selection instanceof StructuredSelection )
    {
      final StructuredSelection strSelection = (StructuredSelection) selection;
      final Object element = strSelection.getFirstElement();

      getPreferenceStore().setValue( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_FORMAT, element.toString() );
    }

    final IPreferenceStore iStore = getPreferenceStore();
    if( iStore instanceof PreferenceStore )
    {
      final PreferenceStore store = (PreferenceStore) iStore;
      try
      {
        store.save();
      }
      catch( final IOException e )
      {
        e.printStackTrace();
      }
    }

    return super.performOk();
  }
}
