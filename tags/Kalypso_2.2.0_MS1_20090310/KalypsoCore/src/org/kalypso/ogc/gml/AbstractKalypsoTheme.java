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

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang.ObjectUtils;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ISafeRunnable;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.PlatformObject;
import org.eclipse.core.runtime.SafeRunner;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Device;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.kalypso.commons.i18n.I10nString;
import org.kalypso.contribs.eclipse.core.runtime.SafeRunnable;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.core.KalypsoCoreExtensions;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.core.catalog.CatalogManager;
import org.kalypso.core.catalog.ICatalog;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * <p>
 * Abstract implementation of IKalypsoTheme
 * </p>
 * <p>
 * Implements common features to all KalypsoTheme's
 * </p>
 *
 * @author Gernot Belger
 */
public abstract class AbstractKalypsoTheme extends PlatformObject implements IKalypsoTheme
{
  private static interface IListenerRunnable
  {
    public void visit( final IKalypsoThemeListener l );
  }

  protected static final Object[] EMPTY_CHILDREN = new Object[] {};

  private final Collection<IKalypsoThemeListener> m_listeners = new HashSet<IKalypsoThemeListener>();

  private final Map<String, String> m_properties = Collections.synchronizedMap( new HashMap<String, String>() );

  private final IMapModell m_mapModel;

  /**
   * Stores the relative URL or an URN for an icon, which can be used for the layer in a legend. May be null.
   */
  private String m_legendIcon = null;

  /**
   * The context, if the theme is part of a template loaded from a file. May be null. Used to resolve dependend
   * resources like the legend-icon.
   */
  private URL m_context = null;

  /**
   * Stores an icon from an external URL or URN and which can be used for the layer in a legend. May be null.
   */
  private org.eclipse.swt.graphics.Image m_externIcon = null;

  private I10nString m_name;

  private String m_type;

  /**
   * The status of this theme. Should be set of implementing classes whenever something unexpected occurs (e.g. error
   * while loading the theme, ...).
   */
  private IStatus m_status = Status.OK_STATUS;

  private boolean m_isVisible = true;

  /**
   * Holds the descriptor for the default icon of all themes. Is used in legends, such as the outline.
   */
  private org.eclipse.swt.graphics.Image m_standardThemeIcon;

  /**
   * True, if the theme should show its children in an outline. Otherwise false.
   */
  private boolean m_showLegendChildren = true;

  /**
   * The constructor.
   *
   * @param name
   *          The name of the theme.
   * @param type
   *          The type of the theme.
   * @param mapModel
   *          The map model to use.
   * @param legendIcon
   *          Stores the relative URL or an URN for an icon, which can be used for the layer in a legend. May be null.
   * @param context
   *          The context, if the theme is part of a template loaded from a file. May be null.
   * @param shouldShowChildren
   *          True, if the theme should show its children in an outline. Otherwise false.
   */
  public AbstractKalypsoTheme( final I10nString name, final String type, final IMapModell mapModel )
  {
    Assert.isNotNull( mapModel );

    m_name = name;
    m_type = type;
    m_mapModel = mapModel;

    /* Initialize properties */
    // deleteable defaults to 'true', because this was the old behavior
    m_properties.put( IKalypsoTheme.PROPERTY_DELETEABLE, Boolean.toString( true ) );
  }

  /**
   * Runns the given runnable on every listener in a safe way.
   */
  private void acceptListenersRunnable( final IListenerRunnable r )
  {
    final IKalypsoThemeListener[] listeners = m_listeners.toArray( new IKalypsoThemeListener[m_listeners.size()] );
    for( final IKalypsoThemeListener l : listeners )
    {
      final ISafeRunnable code = new SafeRunnable()
      {
        public void run( ) throws Exception
        {
          r.visit( l );
        }
      };

      SafeRunner.run( code );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#addKalypsoThemeListener(org.kalypso.ogc.gml.IKalypsoThemeListener)
   */
  public void addKalypsoThemeListener( final IKalypsoThemeListener listener )
  {
    Assert.isNotNull( listener );

    m_listeners.add( listener );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#dispose()
   */
  public void dispose( )
  {
    m_listeners.clear();

    if( m_standardThemeIcon != null )
    {
      m_standardThemeIcon.dispose();
    }

    if( m_externIcon != null )
    {
      m_externIcon.dispose();
    }
  }

  /**
   * Fire the given event to my registered listeners.
   */
  protected void fireContextChanged( )
  {
    acceptListenersRunnable( new IListenerRunnable()
    {
      public void visit( final IKalypsoThemeListener l )
      {
        if( l == null )
          return;

        l.contextChanged( AbstractKalypsoTheme.this );
      }
    } );
  }

  /**
   * Fire the given event to my registered listeners.
   */
  protected void fireStatusChanged( )
  {
    acceptListenersRunnable( new IListenerRunnable()
    {
      public void visit( final IKalypsoThemeListener l )
      {
        if( l == null )
          return;

        l.statusChanged( AbstractKalypsoTheme.this );
      }
    } );
  }

  /**
   * Fire the given event to my registered listeners.
   */
  protected void fireVisibilityChanged( final boolean newVisibility )
  {
    acceptListenersRunnable( new IListenerRunnable()
    {
      public void visit( final IKalypsoThemeListener l )
      {
        if( l == null )
          return;

        l.visibilityChanged( AbstractKalypsoTheme.this, newVisibility );
      }
    } );
  }

  /**
   * Fire the given event to my registered listeners.<br>
   *
   * @param invalidExtent
   *          The extent that is no more valid; <code>null</code> indicating that the complete theme should be
   *          repainted.
   */
  protected void fireRepaintRequested( final GM_Envelope invalidExtent )
  {
    acceptListenersRunnable( new IListenerRunnable()
    {
      public void visit( final IKalypsoThemeListener l )
      {
        if( l == null )
          return;

        l.repaintRequested( AbstractKalypsoTheme.this, invalidExtent );
      }
    } );
  }

  /**
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getChildren(java.lang.Object)
   */
  public Object[] getChildren( final Object o )
  {
    return AbstractKalypsoTheme.EMPTY_CHILDREN;
  }

  /**
   * Returns the type of the theme by default. Override if needed.
   *
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getContext()
   */
  public String getTypeContext( )
  {
    return getType();
  }

  /**
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getImageDescriptor(java.lang.Object)
   */
  public ImageDescriptor getImageDescriptor( final Object object )
  {
    final IStatus status = getStatus();
    if( !status.isOK() )
    {
      final ISharedImages sharedImages = PlatformUI.getWorkbench().getSharedImages();

      switch( status.getSeverity() )
      {
        case IStatus.ERROR:
          return sharedImages.getImageDescriptor( "IMG_OBJS_ERROR_PATH" ); //$NON-NLS-1$
        case IStatus.WARNING:
          return sharedImages.getImageDescriptor( "IMG_OBJS_WARNING_PATH" ); //$NON-NLS-1$
        case IStatus.INFO:
          return sharedImages.getImageDescriptor( "IMG_OBJS_INFO_PATH" ); //$NON-NLS-1$
      }
    }

    return getOutlineImageFromLayer();
  }

  /**
   * This function returns the icon set in the style (StyledLayerType), if any.<br>
   * This may be icons with a relative path or icons, which are defined via some URNs.<br>
   *
   * @return If an user icon or URN is defined, this icon will be returned.<br>
   *         If not, it checks the number of styles and rules.<br>
   *         If only one style and rule exists, there is a generated icon returned, representing the first rule.<br>
   *         If there are more rules or styles, there is the standard icon returned, defined by each theme itself.
   */
  private ImageDescriptor getOutlineImageFromLayer( )
  {
    /* Make a default icon for the legend. */
    if( m_legendIcon == null )
    {
      final Object[] styles = getChildren( this );
      if( styles instanceof ThemeStyleTreeObject[] && styles.length == 1 )
      {
        /* Cast ... . */
        final ThemeStyleTreeObject[] themeStyles = (ThemeStyleTreeObject[]) styles;

        /* One must exist here! */
        final ThemeStyleTreeObject style = themeStyles[0];

        /* Get the rules. */
        final Object[] rules = style.getChildren( style );
        if( rules.length == 1 )
        {
          final RuleTreeObject rule = (RuleTreeObject) rules[0];
          return rule.getImageDescriptor( rule );
        }

        /* Otherwise there are more styles or rules, so give them the default image. */
        return getDefaultIcon();
      }

      return getDefaultIcon();
    }

    /* If the image is disposed; this theme was disposed */
    if( m_externIcon != null && m_externIcon.isDisposed() )
      return null;

    /* If the m_legendIcon string was already evaluated and an image does exist, return this image. */
    if( m_externIcon != null && !m_externIcon.isDisposed() )
      return ImageDescriptor.createFromImage( m_externIcon );

    /* Check, if it is a special URN. */
    final Pattern p = Pattern.compile( "^urn:kalypso:map:theme:swtimage:style:(.*):rule:(.*)$", Pattern.MULTILINE ); //$NON-NLS-1$
    final Matcher m = p.matcher( m_legendIcon.trim() );

    /* A special URN was defined. Evaluate it. */
    if( m.matches() && m.groupCount() == 2 )
    {
      final String styleName = m.group( 1 );
      final String ruleName = m.group( 2 );

      final Object[] children = getChildren( this );
      if( children instanceof ThemeStyleTreeObject[] )
      {
        final ThemeStyleTreeObject[] styles = (ThemeStyleTreeObject[]) children;
        for( final ThemeStyleTreeObject style : styles )
        {
          final String sName = style.getStyle().getName();
          if( styleName.equals( sName ) )
          {
            final Object[] children2 = style.getChildren( style );
            if( children2 instanceof RuleTreeObject[] )
            {
              final RuleTreeObject[] rules = (RuleTreeObject[]) children2;
              for( final RuleTreeObject ruleTreeObject : rules )
              {
                final String rName = ruleTreeObject.getRule().getName();
                if( ruleName.equals( rName ) )
                {
                  /* Found the right one, need this image icon. */
                  final ImageDescriptor descriptor = ruleTreeObject.getImageDescriptor( ruleTreeObject );

                  /* Create the Image. */
                  m_externIcon = descriptor.createImage();

                  /* Need a new one with the created image, because the image is cached and should be disposed. */
                  /* Using the descriptor above will result in undisposed images. */
                  return ImageDescriptor.createFromImage( m_externIcon );
                }
              }
            }

            break;
          }
        }
      }

      return getDefaultIcon();
    }

    /* Resolve the URL. */
    final URL absoluteUrl = getLegendIconURL();

    /* On error, return the default icon. */
    if( absoluteUrl == null )
      return getDefaultIcon();

    /* Create the descriptor. */
    final ImageDescriptor descriptor = ImageDescriptor.createFromURL( absoluteUrl );

    /* Create the Image. */
    m_externIcon = descriptor.createImage();

    /* Need a new one with the created image, because the image is cached and should be disposed. */
    /* Using the descriptor above will result in undisposed images. */
    return ImageDescriptor.createFromImage( m_externIcon );
  }

  /**
   * This function returns the resolved URL for the legend icon or null, if none could be created.
   *
   * @return The resolved URL for the legend icon or null, if none could be created.
   */
  private URL getLegendIconURL( )
  {
    try
    {
      /* A URL or URN was given. */
      if( m_legendIcon.startsWith( "urn" ) ) //$NON-NLS-1$
      {
        // search for url
        final CatalogManager catalogManager = KalypsoCorePlugin.getDefault().getCatalogManager();
        final ICatalog baseCatalog = catalogManager.getBaseCatalog();
        if( baseCatalog == null )
          return null;

        final String uri = baseCatalog.resolve( m_legendIcon, m_legendIcon );
        if( uri == null || uri.equals( m_legendIcon ) )
          return null;

        /* Resolve the URL. */
        final URL absoluteUrl = new URL( uri );

        return absoluteUrl;
      }

      /* Resolve the URL. */
      final URL absoluteUrl = UrlResolverSingleton.resolveUrl( m_context, m_legendIcon );

      return absoluteUrl;
    }
    catch( final MalformedURLException e )
    {
      KalypsoCorePlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );

      return null;
    }
  }

  /**
   * This function should return the default image descriptor representing the theme. <br>
   * Subclasses should override, if they want an own standard icon for representing them.<br>
   * <strong>Note:</strong><br>
   * <br>
   * This has only an effect, if the user does not define an URL or URN and the theme has more then one style or rule.
   *
   * @return The default image descriptor.
   */
  protected ImageDescriptor getDefaultIcon( )
  {
    if( m_standardThemeIcon == null )
    {
      m_standardThemeIcon = new Image( Display.getCurrent(), getClass().getResourceAsStream( "resources/standardTheme.gif" ) ); //$NON-NLS-1$
    }

    return ImageDescriptor.createFromImage( m_standardThemeIcon );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getLabel()
   */
  public String getLabel( )
  {
    return getName().getValue();
  }

  /**
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getLabel(java.lang.Object)
   */
  public String getLabel( final Object o )
  {
    final StringBuffer sb = new StringBuffer();

    final I10nString themeName = getName();

    sb.append( themeName.getValue() );

    final IStatus status = getStatus();
    if( !status.isOK() )
    {
      sb.append( " - " ); //$NON-NLS-1$
      sb.append( status.getMessage() );
    }

    return sb.toString();
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getLegendGraphic(org.eclipse.swt.graphics.Font)
   */
  @SuppressWarnings("unused")
  public org.eclipse.swt.graphics.Image getLegendGraphic( final org.eclipse.swt.graphics.Font font ) throws CoreException
  {
    final int BORDER = 0;
    final int ICON_SIZE = 16;
    final int GAP = 4;

    final String label = getLabel( this );

    final Device device = font.getDevice();
    final Point textExtent = calcTextSize( label, font );

    final int width = BORDER + ICON_SIZE + GAP + textExtent.x;
    final int height = Math.max( 16, textExtent.y );

    /* Create the image. */
    final org.eclipse.swt.graphics.Image image = new org.eclipse.swt.graphics.Image( device, width, height );

    /* Need a graphical context. */
    final GC gc = new GC( image );

    /* Set the font. */
    gc.setFont( font );

    /* Change the color. */
    gc.setForeground( gc.getDevice().getSystemColor( SWT.COLOR_BLACK ) );

    /* Get the icon. */
    final ImageDescriptor descriptor = getImageDescriptor( this );

    /* Draw the icon. */
    final org.eclipse.swt.graphics.Image icon = descriptor.createImage();
    gc.drawImage( icon, BORDER, 0 );
    icon.dispose();

    /* Draw the text. */
    gc.drawString( label, BORDER + ICON_SIZE + GAP, 0, true );

    gc.dispose();

    return image;
  }

  // TODO: move into helper class
  public static Point calcTextSize( final String label, final Font font )
  {
    final org.eclipse.swt.graphics.Image tmpImage = new org.eclipse.swt.graphics.Image( font.getDevice(), 1, 1 );
    final GC tmpGC = new GC( tmpImage );
    tmpGC.setFont( font );
    final Point textExtent = tmpGC.textExtent( label );
    tmpGC.dispose();
    tmpImage.dispose();
    return textExtent;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getMapModell()
   */
  public IMapModell getMapModell( )
  {
    return m_mapModel;
  }

  public I10nString getName( )
  {
    return m_name;
  }

  /**
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getParent(java.lang.Object)
   */
  public Object getParent( final Object o )
  {
    Assert.isTrue( o == this );

    return m_mapModel.getThemeParent( this );
  }

  /**
   * @return <code>defaultValue</code>, if the requested property is not set.
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getProperty(java.lang.String, java.lang.String)
   */
  public String getProperty( final String name, final String defaultValue )
  {
    if( !m_properties.containsKey( name ) )
      return defaultValue;

    return m_properties.get( name );
  }

  /**
   * Return the names of all known properties.
   */
  public String[] getPropertyNames( )
  {
    return m_properties.keySet().toArray( new String[m_properties.keySet().size()] );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getStatus()
   */
  public IStatus getStatus( )
  {
    return m_status;
  }

  public String getType( )
  {
    return m_type;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#isLoaded()
   */
  public boolean isLoaded( )
  {
    return true;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#isVisible()
   */
  public boolean isVisible( )
  {
    return m_isVisible;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#removeKalypsoThemeListener(org.kalypso.ogc.gml.IKalypsoThemeListener)
   */
  public void removeKalypsoThemeListener( final IKalypsoThemeListener listener )
  {
    m_listeners.remove( listener );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#setName(org.kalypso.contribs.java.lang.I10nString)
   */
  public void setName( final I10nString name )
  {
    m_name = name;
    setStatus( Status.OK_STATUS );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#setProperty(java.lang.String, java.lang.String)
   */
  public void setProperty( final String name, final String value )
  {
    m_properties.put( name, value );

    // REMARK: we use status changed at the moment, maybe we should fire a special event for properties?
    fireStatusChanged();
  }

  public void setStatus( final IStatus status )
  {
    // Do not fire change if status did not change really; else we may get endless loop
    if( ObjectUtils.equals( status.getSeverity(), m_status.getSeverity() ) && ObjectUtils.equals( status.getMessage(), m_status.getMessage() ) )
      return;

    m_status = status;

    fireStatusChanged();
  }

  public void setType( final String type )
  {
    m_type = type;

    fireStatusChanged();
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#setVisible(boolean)
   */
  public void setVisible( final boolean visible )
  {
    if( visible != m_isVisible )
    {
      m_isVisible = visible;
      fireVisibilityChanged( visible );
    }
  }

  @Override
  public String toString( )
  {
    return getLabel( this );
  }

  /**
   * @see org.eclipse.core.runtime.PlatformObject#getAdapter(java.lang.Class)
   */
  @SuppressWarnings("unchecked")
  @Override
  public Object getAdapter( final Class adapter )
  {
    if( adapter == IKalypsoThemeInfo.class )
    {
      /* If an explicit info is configured for this map, use it */
      final String themeInfoId = getProperty( IKalypsoTheme.PROPERTY_THEME_INFO_ID, null );
      if( themeInfoId != null )
        return KalypsoCoreExtensions.createThemeInfo( themeInfoId, this );
    }

    return super.getAdapter( adapter );
  }

  /**
   * This function returns the URL or URN defined by the user for an icon, which should be displayed in a legend or an
   * outline.
   *
   * @return The URL or URN string. May be null.
   */
  public String getLegendIcon( )
  {
    return m_legendIcon;
  }

  public void setLegendIcon( final String legendIcon, final URL context )
  {
    if( ObjectUtils.equals( m_legendIcon, legendIcon ) && ObjectUtils.equals( m_context, context ) )
      return;

    m_legendIcon = legendIcon;
    m_context = context;

    /* If the image is disposed; this theme was disposed */
    if( m_externIcon != null && m_externIcon.isDisposed() )
    {
      m_externIcon.dispose();
      m_externIcon = null;
    }

    fireStatusChanged();
  }

  /**
   * This function returns the context.
   *
   * @return The context, if the theme is part of a template loaded from a file. May be null.
   */
  protected URL getContext( )
  {
    return m_context;
  }

  /**
   * This function returns true, if the theme allows showing its children in an outline. Otherwise, it will return
   * false.
   *
   * @return True,if the theme allows showing its children in an outline. Otherwise, false.
   */
  public boolean shouldShowLegendChildren( )
  {
    return m_showLegendChildren;
  }

  public void setShowLegendChildren( final boolean showChildren )
  {
    if( ObjectUtils.equals( m_showLegendChildren, showChildren ) )
      return;

    m_showLegendChildren = showChildren;

    fireStatusChanged();
  }

  /**
   * @see org.kalypso.ogc.gml.ICheckStateProvider#isChecked()
   */
  @Override
  public boolean isChecked( )
  {
    return m_isVisible;
  }

  /**
   * @see org.kalypso.ogc.gml.ICheckStateProvider#isGrayed()
   */
  @Override
  public boolean isGrayed( )
  {
    return false;
  }

}