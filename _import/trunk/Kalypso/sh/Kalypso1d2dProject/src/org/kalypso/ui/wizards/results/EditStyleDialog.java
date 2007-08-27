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
package org.kalypso.ui.wizards.results;

import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.contribs.java.net.IUrlResolver2;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;
import org.kalypsodeegree.graphics.sld.NamedLayer;
import org.kalypsodeegree.graphics.sld.PointSymbolizer;
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
  private static final String SETTINGS_SECTION = "ResultStyleEditorDialogSettings";

  private static final String SETTINGS_X = "posx";

  private static final String SETTINGS_Y = "posy";

  private final IFile m_sldFile;

  private IDialogSettings m_dialogSettings;

  private double m_minValue;

  private double m_maxValue;

  public EditStyleDialog( Shell parentShell, IFile sldFile )
  {
    super( parentShell );
    m_sldFile = sldFile;

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
    /* identify style */
    Symbolizer symbolizer = processStyle();

    /* choose the composite, depending on the style */
    if( symbolizer instanceof SurfaceLineSymbolizer )
    {
      SurfaceLineSymbolizer symb = (SurfaceLineSymbolizer) symbolizer;
      final LineColorMap colorMap = symb.getColorMap();
      LineColorMapEditorComposite comp = new LineColorMapEditorComposite( parent, SWT.NONE, colorMap );
    }
    else if( symbolizer instanceof SurfacePolygonSymbolizer )
    {
      SurfacePolygonSymbolizer symb = (SurfacePolygonSymbolizer) symbolizer;
      final PolygonColorMap colorMap = symb.getColorMap();

      PolygonColorMapEditorComposite comp = new PolygonColorMapEditorComposite( parent, SWT.NONE, colorMap );
    }
    else if( symbolizer instanceof PointSymbolizer )
    {
      PointSymbolizer symb = (PointSymbolizer) symbolizer;
      symb.getGraphic();

    }
    return null;
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#okPressed()
   */
  @Override
  protected void okPressed( )
  {
    // write the style back to file

    super.okPressed();
  }

  private Symbolizer processStyle( )
  {

    InputStream inputStream = null;
    try
    {
      inputStream = m_sldFile.getContents();
      final IUrlResolver2 resolver = new IUrlResolver2()
      {
        @SuppressWarnings("synthetic-access")
        public URL resolveURL( String relativeOrAbsolute ) throws MalformedURLException
        {
          URL url = m_sldFile.getLocationURI().toURL();

          return url;
        }

      };
      final StyledLayerDescriptor sld = SLDFactory.createSLD( resolver, inputStream );

      final NamedLayer[] namedLayers = sld.getNamedLayers();
      // get always just the first layer
      final NamedLayer namedLayer = namedLayers[0];
      final String layerName = namedLayer.getName();

      // get always the first style (we assume there is only one)
      final Style[] styles = namedLayer.getStyles();

      final Style style = styles[0];
      if( style instanceof UserStyle )
      {
        final String styleName = style.getName();
        final UserStyle userStyle = (UserStyle) style;
        final FeatureTypeStyle[] featureTypeStyles = userStyle.getFeatureTypeStyles();
        // we assume, that there is only one feature type style and take the first we can get.
        final FeatureTypeStyle featureTypeStyle = featureTypeStyles[0];
        final String featureTypeStyleName = featureTypeStyle.getName();

        // we assume, that there is only one rule and take the first we can get.
        final Rule[] rules = featureTypeStyle.getRules();
        final Rule rule = rules[0];

        // and the first and only symbolizer is taken
        final Symbolizer[] symbolizers = rule.getSymbolizers();
        final Symbolizer symb = symbolizers[0];

        return symb;
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

    getShell().setText( "Style Manager" );
    setTitle( "Bearbeitung der Style-Vorlagen." );
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

}
