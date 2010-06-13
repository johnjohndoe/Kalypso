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
package org.kalypso.ui.wizards.results.editor;

import java.io.InputStream;
import java.math.BigDecimal;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.io.IOUtils;
import org.apache.tools.ant.filters.StringInputStream;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.java.net.IUrlResolver2;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.ui.editor.sldEditor.LineColorMapEditorComposite;
import org.kalypso.ui.editor.sldEditor.PolygonColorMapEditorComposite;
import org.kalypso.ui.wizards.i18n.Messages;
import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;
import org.kalypsodeegree.graphics.sld.NamedLayer;
import org.kalypsodeegree.graphics.sld.PointSymbolizer;
import org.kalypsodeegree.graphics.sld.PolygonColorMapEntry;
import org.kalypsodeegree.graphics.sld.Rule;
import org.kalypsodeegree.graphics.sld.Style;
import org.kalypsodeegree.graphics.sld.StyledLayerDescriptor;
import org.kalypsodeegree.graphics.sld.SurfaceLineSymbolizer;
import org.kalypsodeegree.graphics.sld.SurfacePolygonSymbolizer;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.graphics.sld.UserStyle;
import org.kalypsodeegree.xml.XMLParsingException;
import org.kalypsodeegree_impl.graphics.sld.LineColorMap;
import org.kalypsodeegree_impl.graphics.sld.PolygonColorMap;
import org.kalypsodeegree_impl.graphics.sld.SLDFactory;

/**
 * @author Thomas Jung
 * 
 */
public class EditStyleDialog extends TitleAreaDialog
{
  private final Set<IEditStyleDialogModifyListener> m_listeners = new HashSet<IEditStyleDialogModifyListener>();

  private static final String SETTINGS_SECTION = "ResultStyleEditorDialogSettings"; //$NON-NLS-1$

  private static final String SETTINGS_X = "posx"; //$NON-NLS-1$

  private static final String SETTINGS_Y = "posy"; //$NON-NLS-1$

  private IFile m_sldFile;

  private String m_newSldFileName;

  private IDialogSettings m_dialogSettings;

  private final BigDecimal m_minValue;

  private final BigDecimal m_maxValue;

  private final Pattern m_patternFileName = Pattern.compile( "[a-zA-Z0-9_]+" ); //$NON-NLS-1$

  private StyledLayerDescriptor m_sld;

  private final String m_fileName;

  private Symbolizer[] m_symbolizer;

  private final IFolder m_sldFolder;

  public EditStyleDialog( Shell parentShell, IFile sldFile, BigDecimal minValue, BigDecimal maxValue )
  {
    super( parentShell );

    m_sldFile = sldFile;
    m_maxValue = maxValue;
    m_minValue = minValue;
    m_fileName = FileUtilities.nameWithoutExtension( m_sldFile.getName() );

    m_sldFolder = (IFolder) m_sldFile.getParent();

    final IDialogSettings dialogSettings = KalypsoModel1D2DPlugin.getDefault().getDialogSettings();
    m_dialogSettings = dialogSettings.getSection( SETTINGS_SECTION );
    if( m_dialogSettings == null )
      m_dialogSettings = dialogSettings.addNewSection( SETTINGS_SECTION );

    if( m_dialogSettings.get( SETTINGS_X ) == null )
      m_dialogSettings.put( SETTINGS_X, -1 );

    if( m_dialogSettings.get( SETTINGS_Y ) == null )
      m_dialogSettings.put( SETTINGS_Y, -1 );

    setShellStyle( getShellStyle() | SWT.RESIZE );
  }

  @Override
  protected Control createDialogArea( final Composite parent )
  {
    final Composite commonComposite = new Composite( parent, SWT.NONE );
    GridData gridDataCommon = new GridData( SWT.FILL, SWT.FILL, true, true );
    commonComposite.setLayoutData( gridDataCommon );
    commonComposite.setLayout( new GridLayout( 2, false ) );

    createFileManagerComponent( commonComposite );

    createStyleComponent( commonComposite );

    return commonComposite;
  }

  private void createFileManagerComponent( final Composite commonComposite )
  {
    /* file manager */

    // file name
    final Label fileNameLabel = new Label( commonComposite, SWT.NONE );
    fileNameLabel.setText( Messages.getString("org.kalypso.ui.wizards.results.editor.EditStyleDialog.0") ); //$NON-NLS-1$
    fileNameLabel.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, false, false ) );

    final Text fileNameText = new Text( commonComposite, SWT.BORDER | SWT.TRAIL );
    fileNameText.setText( m_fileName );
    GridData gridDataFileNameText = new GridData( SWT.BEGINNING, SWT.CENTER, true, false );
    gridDataFileNameText.widthHint = 140;
    fileNameText.setLayoutData( gridDataFileNameText );

    fileNameText.addKeyListener( new KeyAdapter()
    {

      @SuppressWarnings("synthetic-access")
      @Override
      public void keyPressed( final KeyEvent event )
      {
        switch( event.keyCode )
        {
          case SWT.CR:
            checkFileNameText( commonComposite, fileNameText );
        }
      }
    } );

    fileNameText.addFocusListener( new FocusListener()
    {
      @Override
      @SuppressWarnings("synthetic-access")
      public void focusGained( final FocusEvent e )
      {
        checkFileNameText( commonComposite, fileNameText );
      }

      @Override
      @SuppressWarnings("synthetic-access")
      public void focusLost( final FocusEvent e )
      {
        checkFileNameText( commonComposite, fileNameText );
      }
    } );

    fileNameText.addModifyListener( new ModifyListener()
    {

      @Override
      @SuppressWarnings("synthetic-access")
      public void modifyText( final ModifyEvent e )
      {
        checkFileNameText( commonComposite, fileNameText );
      }
    } );
  }

  private void createStyleComponent( final Composite commonComposite )
  {
    m_symbolizer = parseStyle();
    /* choose the composite, depending on the style */
    if( m_symbolizer[0] instanceof SurfaceLineSymbolizer )
    {
      SurfaceLineSymbolizer symb = (SurfaceLineSymbolizer) m_symbolizer[0];
      final LineColorMap colorMap = symb.getColorMap();
      if( colorMap.getColorMap().length > 0 )
      {
        LineColorMapEditorComposite comp = new LineColorMapEditorComposite( commonComposite, SWT.NONE, colorMap, m_minValue, m_maxValue );
        GridData gridDataComp = new GridData( SWT.FILL, SWT.FILL, true, true );
        gridDataComp.horizontalSpan = 2;
        comp.setLayoutData( gridDataComp );
      }
      else
      {
        final Text errorText = new Text( commonComposite, SWT.NONE );
        errorText.setText( Messages.getString("org.kalypso.ui.wizards.results.editor.EditStyleDialog.5") ); //$NON-NLS-1$
        errorText.setBackground( commonComposite.getBackground() );
      }
    }
    else if( m_symbolizer[0] instanceof SurfacePolygonSymbolizer )
    {
      SurfacePolygonSymbolizer symb = (SurfacePolygonSymbolizer) m_symbolizer[0];
      final PolygonColorMap colorMap = symb.getColorMap();
      final PolygonColorMapEntry[] colorMapEntries = colorMap.getColorMap();
      if( colorMapEntries.length > 0 )
      {
        final PolygonColorMapEntry fromEntry = colorMapEntries[0];
        final PolygonColorMapEntry toEntry = colorMapEntries[colorMapEntries.length - 1];

        PolygonColorMapEditorComposite comp = new PolygonColorMapEditorComposite( commonComposite, SWT.NONE, fromEntry, toEntry, m_minValue, m_maxValue )
        {
          @Override
          protected void colorMapChanged( )
          {
            final List<PolygonColorMapEntry> colorMapList = getColorMap();
            if( colorMapList.size() > 0 )
              colorMap.replaceColorMap( colorMapList );
          }
        };
        GridData gridDataComp = new GridData( SWT.FILL, SWT.FILL, true, true );
        gridDataComp.horizontalSpan = 2;
        comp.setLayoutData( gridDataComp );
      }
      else
      {
        final Text errorText = new Text( commonComposite, SWT.NONE );
        errorText.setText( Messages.getString("org.kalypso.ui.wizards.results.editor.EditStyleDialog.6") ); //$NON-NLS-1$
        errorText.setBackground( commonComposite.getBackground() );
      }
    }
    else if( m_symbolizer[0] instanceof PointSymbolizer )
    {
      PointSymbolizer symb = (PointSymbolizer) m_symbolizer[0];
      VectorEditorComposite comp = new VectorEditorComposite( commonComposite, SWT.NONE, symb, m_minValue, m_maxValue );
      GridData gridDataComp = new GridData( SWT.FILL, SWT.FILL, true, true );
      gridDataComp.horizontalSpan = 2;
      comp.setLayoutData( gridDataComp );
    }
    else
    {
      final Text errorText1 = new Text( commonComposite, SWT.NONE );
      GridData gridDataText1 = new GridData( SWT.BEGINNING, SWT.CENTER, true, true );
      gridDataText1.horizontalSpan = 2;
      gridDataText1.widthHint = 400;

      errorText1.setLayoutData( gridDataText1 );
      errorText1.setText( Messages.getString("org.kalypso.ui.wizards.results.editor.EditStyleDialog.7") ); //$NON-NLS-1$
      errorText1.setBackground( commonComposite.getBackground() );

      final Text errorText2 = new Text( commonComposite, SWT.NONE );
      errorText2.setLayoutData( gridDataText1 );
      errorText2.setText( Messages.getString("org.kalypso.ui.wizards.results.editor.EditStyleDialog.8") ); //$NON-NLS-1$
      errorText2.setBackground( commonComposite.getBackground() );
    }
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#okPressed()
   */
  @Override
  protected void okPressed( )
  {
    // write the style back to file
    final String sldXML = m_sld.exportAsXML();
    final String sldXMLwithHeader = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" + sldXML; //$NON-NLS-1$

    try
    {
      if( m_sldFile.exists() )
        m_sldFile.setContents( new StringInputStream( sldXMLwithHeader, "UTF-8" ), false, true, new NullProgressMonitor() ); //$NON-NLS-1$
      else
      {
        m_sldFile.create( new StringInputStream( sldXMLwithHeader, "UTF-8" ), false, new NullProgressMonitor() ); //$NON-NLS-1$
      }

      if( m_newSldFileName != null && m_newSldFileName != m_fileName )
        updateSldFile();

    }
    catch( CoreException e )
    {
      // TODO Auto-generated catch block e.printStackTrace();
      e.printStackTrace();
    }

    super.okPressed();
  }

  private Symbolizer[] parseStyle( )
  {
    InputStream inputStream = null;
    try
    {
      inputStream = m_sldFile.getContents();
      final IUrlResolver2 resolver = new IUrlResolver2()
      {
        @Override
        @SuppressWarnings("synthetic-access")
        public URL resolveURL( String relativeOrAbsolute ) throws MalformedURLException
        {
          final URL sldURL = ResourceUtilities.createURL( m_sldFile );
          return new URL( sldURL, relativeOrAbsolute );
        }

      };
      m_sld = SLDFactory.createSLD( resolver, inputStream );
      final NamedLayer[] namedLayers = m_sld.getNamedLayers();
      // get always just the first layer
      final NamedLayer namedLayer = namedLayers[0];

      // get always the first style (we assume there is only one)
      final Style[] styles = namedLayer.getStyles();

      final Style style = styles[0];
      if( style instanceof UserStyle )
      {
        final UserStyle userStyle = (UserStyle) style;
        final FeatureTypeStyle[] featureTypeStyles = userStyle.getFeatureTypeStyles();
        // we assume, that there is only one feature type style and take the first we can get.
        final FeatureTypeStyle featureTypeStyle = featureTypeStyles[0];

        // we assume, that there is only one rule and take the first we can get.
        final Rule[] rules = featureTypeStyle.getRules();

        final List<Symbolizer> symbList = new ArrayList<Symbolizer>();

        for( Rule rule : rules )
        {
          final Symbolizer[] symbolizers = rule.getSymbolizers();
          // and the first and only symbolizer is taken
          final Symbolizer symb = symbolizers[0];
          symbList.add( symb );
        }

        return symbList.toArray( new Symbolizer[symbList.size()] );
      }
    }
    catch( CoreException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch( XMLParsingException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

    finally
    {
      IOUtils.closeQuietly( inputStream );
    }
    return null;
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#create()
   */
  @Override
  public void create( )
  {
    super.create();

    getShell().setText( Messages.getString("org.kalypso.ui.wizards.results.editor.EditStyleDialog.12") ); //$NON-NLS-1$
    setTitle( Messages.getString("org.kalypso.ui.wizards.results.editor.EditStyleDialog.13") ); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#close()
   */
  @Override
  public boolean close( )
  {
    final Shell shell = getShell();
    if( shell == null || shell.isDisposed() )
      return true;

    // save dialog settings
    final Point location = shell.getLocation();
    m_dialogSettings.put( SETTINGS_X, location.x );
    m_dialogSettings.put( SETTINGS_Y, location.y );

    return super.close();
  }

  /**
   * checks the user typed string for the step width value
   * 
   * @param comp
   *            composite of the text field
   * @param text
   *            the text
   */
  private Text checkFileNameText( final Composite comp, final Text text )
  {
    String tempText = text.getText();

    final Matcher m = m_patternFileName.matcher( tempText );

    if( !m.matches() )
    {
      text.setBackground( comp.getDisplay().getSystemColor( SWT.COLOR_RED ) );
      m_newSldFileName = null;
      m_sldFile = null;

      setErrorMessage( Messages.getString("org.kalypso.ui.wizards.results.editor.EditStyleDialog.14") ); //$NON-NLS-1$
      getButton( OK ).setEnabled( false );
    }
    else
    {
      text.setBackground( comp.getDisplay().getSystemColor( SWT.COLOR_WHITE ) );
      m_newSldFileName = tempText;

      // create a new sld file with the new name
      m_sldFile = m_sldFolder.getFile( m_newSldFileName + ".sld" ); //$NON-NLS-1$

      getButton( OK ).setEnabled( true );
      setErrorMessage( null );
    }
    return text;
  }

  private void updateSldFile( )
  {
    // update styledLocation and combo viewer
    fireModified();
  }

  /**
   * Add the listener to the list of listeners. If an identical listeners has already been registered, this has no
   * effect.
   */
  public void addModifyListener( final IEditStyleDialogModifyListener l )
  {
    m_listeners.add( l );
  }

  public void removeModifyListener( final IEditStyleDialogModifyListener l )
  {
    m_listeners.remove( l );
  }

  protected void fireModified( )
  {
    final IEditStyleDialogModifyListener[] ls = m_listeners.toArray( new IEditStyleDialogModifyListener[m_listeners.size()] );
    for( final IEditStyleDialogModifyListener styleModifyListener : ls )
      styleModifyListener.onStyleChanged( this, m_sldFile );
  }

}
