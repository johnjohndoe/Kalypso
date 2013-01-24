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
package org.kalypso.kalypsomodel1d2d.ui.map.temsys.model;

import java.io.BufferedInputStream;
import java.io.InputStream;
import java.io.StringReader;
import java.net.MalformedURLException;
import java.net.URL;

import org.apache.commons.io.IOUtils;
import org.apache.tools.ant.util.ReaderInputStream;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.java.net.IUrlResolver2;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.ogc.gml.IKalypsoLayerModell;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemeVisitor;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.MapModellHelper;
import org.kalypso.ui.action.AddThemeCommand;
import org.kalypso.ui.editor.mapeditor.AbstractMapPart;
import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;
import org.kalypsodeegree.graphics.sld.NamedLayer;
import org.kalypsodeegree.graphics.sld.Rule;
import org.kalypsodeegree.graphics.sld.Style;
import org.kalypsodeegree.graphics.sld.StyledLayerDescriptor;
import org.kalypsodeegree.graphics.sld.SurfaceLineSymbolizer;
import org.kalypsodeegree.graphics.sld.SurfacePolygonSymbolizer;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.graphics.sld.UserStyle;
import org.kalypsodeegree_impl.graphics.sld.SLDFactory;

/**
 * UI for showing the discretisation model to the user as isolines or categorized areas.
 * 
 * @author Gernot Belger
 */
public class ModelDtmWizard extends Wizard
{
  /** This property is set for the fe-net dtm theme */
  private static final String THEME_PROP_MODEL_DTM = "fenetDtm"; //$NON-NLS-1$

  private CreateModelTinWizardPage m_exportPage;

  private SurfaceIsolineWizardPage m_isolinePage;

  // private SurfacePolygonWizardPage m_polygonPage;

  private final IFile m_dtmFile;

  private final IFEDiscretisationModel1d2d m_discModel;

  private SurfaceLineSymbolizer m_isoSymbolizer;

  private SurfacePolygonSymbolizer m_polySmybolizer;

  private final IFile m_styleFile;

  private StyledLayerDescriptor m_sld;

  private final AbstractMapPart m_mapPart;

  public ModelDtmWizard( final IFile dtmFile, final IFile styleFile, final IFEDiscretisationModel1d2d discModel, final AbstractMapPart mapPart )
  {
    m_dtmFile = dtmFile;
    m_styleFile = styleFile;
    m_discModel = discModel;
    m_mapPart = mapPart;

    setNeedsProgressMonitor( true );
    setWindowTitle( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.temsys.model.ModelDtmWizard.0") ); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#canFinish()
   */
  @Override
  public boolean canFinish( )
  {
    if( getContainer().getCurrentPage() == m_exportPage )
      return false;

    return super.canFinish();
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#addPages()
   */
  @Override
  public void addPages( )
  {
    // read symbolizer file here
    readSymbolizer();

    m_exportPage = new CreateModelTinWizardPage( "exportPage", m_dtmFile, m_discModel ); //$NON-NLS-1$
    m_isolinePage = new SurfaceIsolineWizardPage( "isolinePage", m_isoSymbolizer, m_exportPage ); //$NON-NLS-1$
    // m_polygonPage = new SurfacePolygonWizardPage( "surfacePage", m_polySmybolizer, m_exportPage );

    addPage( m_exportPage );
    addPage( m_isolinePage );
    // addPage( m_polygonPage );
  }

  /**
   * Overwritten in order to create the page laziliy
   * 
   * @see org.eclipse.jface.wizard.Wizard#createPageControls(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPageControls( final Composite pageContainer )
  {
    // do nothing
  }

  private void readSymbolizer( )
  {
    InputStream inputStream = null;
    try
    {
      if( m_styleFile.exists() )
        inputStream = new BufferedInputStream( m_styleFile.getContents() );
      else
        inputStream = getClass().getResourceAsStream( "modelDtm.sld" ); //$NON-NLS-1$

      final URL sldURL = ResourceUtilities.createURL( m_styleFile );
      final IUrlResolver2 resolver = new IUrlResolver2()
      {
        @Override
        public URL resolveURL( final String relativeOrAbsolute ) throws MalformedURLException
        {
          return new URL( sldURL, relativeOrAbsolute );
        }
      };

      m_sld = SLDFactory.createSLD( resolver, inputStream );

      // Find iso and poly smybolizer
      final NamedLayer[] namedLayers = m_sld.getNamedLayers();
      for( final NamedLayer namedLayer : namedLayers )
      {
        // get always the first style (we assume there is only one)
        final Style[] styles = namedLayer.getStyles();
        for( final Style style : styles )
        {
          if( style instanceof UserStyle )
          {
            final UserStyle userStyle = (UserStyle) style;
            final FeatureTypeStyle[] featureTypeStyles = userStyle.getFeatureTypeStyles();
            for( final FeatureTypeStyle featureTypeStyle : featureTypeStyles )
            {
              // we assume, that there is only one rule and take the first we can get.
              final Rule[] rules = featureTypeStyle.getRules();
              for( final Rule rule : rules )
              {
                final Symbolizer[] symbolizers = rule.getSymbolizers();
                for( final Symbolizer symbolizer : symbolizers )
                {
                  if( symbolizer instanceof SurfaceLineSymbolizer && m_isoSymbolizer == null )
                    m_isoSymbolizer = (SurfaceLineSymbolizer) symbolizer;
                  if( symbolizer instanceof SurfacePolygonSymbolizer && m_polySmybolizer == null )
                    m_polySmybolizer = (SurfacePolygonSymbolizer) symbolizer;
                }
              }
            }
          }
        }
      }
    }
    catch( final CoreException e )
    {
      KalypsoModel1D2DPlugin.getDefault().getLog().log( e.getStatus() );
      ErrorDialog.openError( getShell(), getWindowTitle(), Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.temsys.model.ModelDtmWizard.1"), e.getStatus() ); //$NON-NLS-1$
    }
    catch( final Exception e )
    {
      final IStatus status = StatusUtilities.createStatus( IStatus.ERROR, e.getLocalizedMessage(), e );
      KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
      ErrorDialog.openError( getShell(), getWindowTitle(), Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.temsys.model.ModelDtmWizard.2"), status ); //$NON-NLS-1$
    }
    finally
    {
      IOUtils.closeQuietly( inputStream );
    }

  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {

    // save sld
    try
    {
      final String sldXML = m_sld.exportAsXML();
      final String sldXMLwithHeader = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" + sldXML; //$NON-NLS-1$
      final InputStream is = new ReaderInputStream( new StringReader( sldXMLwithHeader ), "UTF-8" ); //$NON-NLS-1$
      if( m_styleFile.exists() )
        m_styleFile.setContents( is, false, true, new NullProgressMonitor() ); //$NON-NLS-1$
      else
        m_styleFile.create( is, false, new NullProgressMonitor() ); //$NON-NLS-1$
    }
    catch( final CoreException e )
    {
      KalypsoModel1D2DPlugin.getDefault().getLog().log( e.getStatus() );
      ErrorDialog.openError( getShell(), getWindowTitle(), Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.temsys.model.ModelDtmWizard.3"), e.getStatus() ); //$NON-NLS-1$
      return true;
    }

    // create/update theme
    final IKalypsoTheme dtmTheme = findDtmTheme();
    if( dtmTheme == null )
    {
      final IKalypsoLayerModell mapModell = (IKalypsoLayerModell) m_mapPart.getMapPanel().getMapModell();

      final URL context = mapModell.getContext();
      final IFile mapFile = ResourceUtilities.findFileFromURL( context );

      final String gmlSource = ResourceUtilities.makeRelativ( mapFile, m_dtmFile ).toString();
      final String sldSource = ResourceUtilities.makeRelativ( mapFile, m_styleFile ).toString();

      final AddThemeCommand addThemeCommand = new AddThemeCommand( mapModell, Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.temsys.model.ModelDtmWizard.4"), "gml", "", gmlSource ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      // addThemeCommand.addStyle( "tinPolyStyle", sldSource );
      addThemeCommand.addStyle( "tinLineStyle", sldSource ); //$NON-NLS-1$
      addThemeCommand.addProperty( THEME_PROP_MODEL_DTM, "true" ); // any value != null //$NON-NLS-1$
      addThemeCommand.addProperty( IKalypsoTheme.PROPERTY_DELETEABLE, "true" ); // any value != null //$NON-NLS-1$

      m_mapPart.postCommand( addThemeCommand, null );
    }
    else
      dtmTheme.setVisible( true );

    return true;
  }

  private IKalypsoTheme findDtmTheme( )
  {
    final IMapModell mapModell = m_mapPart.getMapPanel().getMapModell();
    final IKalypsoTheme[] themes = MapModellHelper.findThemeByProperty( mapModell, THEME_PROP_MODEL_DTM, IKalypsoThemeVisitor.DEPTH_INFINITE );
    if( themes == null || themes.length <= 0 )
      return null;

    return themes[0];
  }
}
