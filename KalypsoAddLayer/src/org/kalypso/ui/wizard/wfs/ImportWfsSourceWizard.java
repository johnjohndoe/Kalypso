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

package org.kalypso.ui.wizard.wfs;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

import javax.naming.OperationNotSupportedException;

import org.apache.commons.io.IOUtils;
import org.deegree.datatypes.QualifiedName;
import org.deegree.ogcwebservices.wfs.capabilities.WFSFeatureType;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.ogc.gml.IKalypsoLayerModell;
import org.kalypso.ogc.gml.loader.WfsLoader;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.KalypsoServiceConstants;
import org.kalypso.ui.action.AddThemeCommand;
import org.kalypso.ui.wizard.IKalypsoDataImportWizard;
import org.kalypsodeegree.filterencoding.ElseFilter;
import org.kalypsodeegree.filterencoding.Filter;
import org.kalypsodeegree.filterencoding.visitor.TransformSRSVisitor;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.tools.FilterUtilites;
import org.kalypsodeegree_impl.filterencoding.ComplexFilter;
import org.kalypsodeegree_impl.filterencoding.FeatureFilter;

/**
 * @author Kuepferle, doemming
 */
public class ImportWfsSourceWizard extends Wizard implements IKalypsoDataImportWizard
{
  private ImportWfsWizardPage m_importWFSPage;

  private ICommandTarget m_outlineviewer;

  private List<String> m_catalog;

  private ImportWfsFilterWizardPage m_filterWFSPage;

  private IKalypsoLayerModell m_modell;

  /**
   * @see org.eclipse.jface.wizard.IWizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    final IKalypsoLayerModell mapModell = m_modell;
    if( mapModell != null )
    {
      try
      {
        final WFSFeatureType[] layers = m_importWFSPage.getChoosenFeatureLayer();
        for( final WFSFeatureType featureType : layers )
        {
          final Filter complexFilter = m_importWFSPage.getFilter( featureType );
          final Filter simpleFilter = m_filterWFSPage.getFilter( featureType );
          final Filter mergedFilter = FilterUtilites.mergeFilters( complexFilter, simpleFilter );

          final String xml;
          if( mergedFilter instanceof ComplexFilter )
            xml = transformToRemoteCRS( (ComplexFilter) mergedFilter, featureType.getDefaultSRS().toString() );
          else if( mergedFilter instanceof FeatureFilter )
            xml = ((FeatureFilter) mergedFilter).toXML().toString();
          else if( mergedFilter instanceof ElseFilter )
            xml = ((ElseFilter) mergedFilter).toXML().toString();
          else
            xml = null;
          // TODO here the featurePath is set to featureMember because this is
          // the top feature of the GMLWorkspace
          // it must be implemented to only set the name of the feature
          // (relative path of feature)

          final StringBuffer source = new StringBuffer();
          final QualifiedName qNameFT = featureType.getName();
          source.append( "#" ).append( WfsLoader.KEY_URL ).append( "=" ).append( m_importWFSPage.getUri() );
          source.append( "#" ).append( WfsLoader.KEY_FEATURETYPE ).append( "=" ).append( qNameFT.getLocalName() );
          final String namespaceURI = qNameFT.getNamespace().toString();
          if( namespaceURI != null && namespaceURI.length() > 0 )
            source.append( "#" ).append( WfsLoader.KEY_FEATURETYPENAMESPACE ).append( "=" ).append( namespaceURI );

          if( xml != null )
            source.append( "#" ).append( WfsLoader.KEY_FILTER ).append( "=" ).append( xml );
          if( m_filterWFSPage.doFilterMaxFeatures() )
          {
            final int maxfeatures = m_filterWFSPage.getMaxFeatures();
            source.append( "#" ).append( WfsLoader.KEY_MAXFEATURE ).append( "=" ).append( Integer.toString( maxfeatures ) );
          }
          final String featurePath = "featureMember[" + qNameFT.getLocalName() + "]";
          String title = featureType.getTitle();
          if( title == null || title.isEmpty() )
            title = qNameFT.getLocalName();
          final AddThemeCommand command = new AddThemeCommand( mapModell, title, "wfs", featurePath, source.toString() );
          m_outlineviewer.postCommand( command, null );
        }
      }
      catch( final OperationNotSupportedException e )
      {
        e.printStackTrace();
        m_filterWFSPage.setErrorMessage( e.getMessage() );
        return false;
      }
      catch( final GM_Exception e )
      {
        e.printStackTrace();
        m_filterWFSPage.setErrorMessage( e.getMessage() );
        return false;
      }
    }
    return true;
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    final InputStream is = getClass().getResourceAsStream( "resources/kalypsoOWS.catalog" );
    try
    {
      // read service catalog file
      readCatalog( is );
      is.close();
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      m_catalog = new ArrayList<String>();
    }
    finally
    {
      IOUtils.closeQuietly( is );
    }
  }

  @Override
  public void addPages( )
  {
    m_importWFSPage = new ImportWfsWizardPage( "WfsImportPage", "Web Feature Service einbinden", ImageProvider.IMAGE_UTIL_UPLOAD_WIZ );
    m_filterWFSPage = new ImportWfsFilterWizardPage( "WfsImportFilterPage", "Filter definieren", ImageProvider.IMAGE_UTIL_IMPORT_WIZARD, m_modell );
    addPage( m_importWFSPage );
    addPage( m_filterWFSPage );
  }

  @Override
  public boolean performCancel( )
  {
    this.dispose();
    return true;
  }

  /**
   * @see org.kalypso.ui.wizard.data.IKalypsoDataImportWizard#setOutlineViewer(org.kalypso.ogc.gml.outline.GisMapOutlineViewer)
   */
  public void setCommandTarget( final ICommandTarget commandTarget )
  {
    m_outlineviewer = commandTarget;
  }

  public List<String> getCatalog( )
  {
    return m_catalog;
  }

  public void readCatalog( final InputStream is ) throws IOException
  {
    final ArrayList<String> catalog = new ArrayList<String>();
    final BufferedReader br = new BufferedReader( new InputStreamReader( is ) );
    String line = br.readLine();
    do
    {
      if( line.startsWith( KalypsoServiceConstants.WFS_LINK_TYPE ) )
        catalog.add( (line.split( "=" ))[1] );

      line = br.readLine();
    }
    while( line != null );

    m_catalog = catalog;
  }

  private String transformToRemoteCRS( final ComplexFilter filter, final String remoteCrs )
  {
    String xml = null;
    final TransformSRSVisitor visitor = new TransformSRSVisitor( remoteCrs );
    visitor.visit( filter.getOperation() );
    xml = filter.toXML().toString();
    return xml;
  }

  /**
   * @see org.kalypso.ui.wizard.IKalypsoDataImportWizard#setMapModel(org.kalypso.ogc.gml.IKalypsoLayerModell)
   */
  public void setMapModel( final IKalypsoLayerModell modell )
  {
    m_modell = modell;
  }

  public void setCatalog( final List<String> catalog )
  {
    m_catalog = catalog;
  }
}