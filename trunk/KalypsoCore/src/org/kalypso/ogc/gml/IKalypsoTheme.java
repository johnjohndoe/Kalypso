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
package org.kalypso.ogc.gml;

import java.awt.Font;
import java.awt.Graphics;
import java.awt.Image;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.ui.model.IWorkbenchAdapter;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * Implements {@link org.eclipse.ui.model.IWorkbenchAdapter} in order to provider nice labels/images, ..... Does NOT
 * implement {@link org.eclipse.ui.model.IWorkbenchAdapter2}. Fonts, and so on are decided outside of the theme scope.
 * 
 * @author Katharina <a href="mailto:k.lupp@web.de>Katharina Lupp </a>
 */
public interface IKalypsoTheme extends IAdaptable, IWorkbenchAdapter
{
  /**
   * Name of the property which determines if the user is allowed to deleted this theme.
   */
  public final static String PROPERTY_DELETEABLE = "deleteable";

  /**
   * Name of the property which determines the id of the IKalypsoThemeInfo for this theme.
   */
  public final static String PROPERTY_THEME_INFO_ID = "themeInfoId";

  /**
   * * Adds a listener to the list of listeners. Has no effect if the same listeners is already registered.
   */
  public void addKalypsoThemeListener( final IKalypsoThemeListener listener );

  /**
   * Removes a listener from the list of listeners. Has no effect if the listeners is not registered.
   */
  public void removeKalypsoThemeListener( final IKalypsoThemeListener listener );

  public void dispose( );

  public void paint( final Graphics g, final GeoTransform p, final double scale, final GM_Envelope bbox, final boolean selected, final IProgressMonitor monitor ) throws CoreException;

  /**
   * returns the name of the layer
   */
  public String getName( );

  public String getType( );

  public void setName( final String name );

  /**
   * Returns the full extent bounding box for the theme.
   */
  public GM_Envelope getFullExtent( );

  public IMapModell getMapModell( );

  /**
   * Returns the context id that this theme represents.
   */
  public String getContext( );

  /**
   * This function should return true, if the theme has tried to load the image, data, etc. once. Regardless if it was
   * successfull or not. In case of a WMS it would return true, if the theme connected to the WMS and the connection was
   * finished. It does not matter if it could successfully retrieve the image or not.
   * 
   * @return True, if the first loading try has finished.
   */
  public boolean isLoaded( );

  public IStatus getStatus( );

  public boolean isVisible( );

  public void setVisible( final boolean visible );

  /**
   * Retrieves the indicated property from this theme.
   * <p>
   * The name of the property should be one of the <code>PROPERTY_</code> constants of this interface.
   * </p>
   * 
   * @param defaultValue
   *            If the property is not set, use this default value.
   * @throws IllegalArgumentException
   *             If the given property name is unknown.
   */
  public String getProperty( final String name, final String defaultValue );

  /**
   * Sets the given property of this theme.<br>
   * The name of the property should be one of the <code>PROPERTY_</code> constants of this interface.
   */
  public void setProperty( final String name, final String value );

  /**
   * This function sets the boundingbox for the new extent.
   * 
   * @param extent
   *            The new extent.
   */
  public void setExtent( int width, int height, GM_Envelope extent );

  /**
   * This function returns an image, containing the legend of the theme, íf one is available. Otherwise it will return
   * null.
   * 
   * @param font
   *            This font will be used for the self created text of the legend.
   * @param layerName
   *            The name of the layer, for which the legend should be loaded.
   * @return An legend graphic or null.
   */
  public Image getLegendGraphic( Font font, String layerName ) throws CoreException;
}