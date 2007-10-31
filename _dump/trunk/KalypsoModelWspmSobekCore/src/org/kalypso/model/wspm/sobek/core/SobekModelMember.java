/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.sobek.core;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import javax.xml.XMLConstants;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.namespace.QName;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;

import nl.wldelft.fews.pi.BranchesComplexType;
import nl.wldelft.fews.pi.CrossSectionsComplexType;
import nl.wldelft.fews.pi.GeoDatumEnumStringType;
import nl.wldelft.fews.pi.LocationsComplexType;
import nl.wldelft.fews.pi.ObjectFactory;
import nl.wldelft.fews.pi.LocationsComplexType.Location;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.NotImplementedException;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.model.wspm.sobek.core.interfaces.IBranch;
import org.kalypso.model.wspm.sobek.core.interfaces.IBranchMaker;
import org.kalypso.model.wspm.sobek.core.interfaces.ICalculationLink;
import org.kalypso.model.wspm.sobek.core.interfaces.ICrossSectionNode;
import org.kalypso.model.wspm.sobek.core.interfaces.ILastfall;
import org.kalypso.model.wspm.sobek.core.interfaces.INode;
import org.kalypso.model.wspm.sobek.core.interfaces.INodeUtils;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekModelMember;
import org.kalypso.model.wspm.sobek.core.interfaces.IWorkspaceInterface;
import org.kalypso.model.wspm.sobek.core.model.Branch;
import org.kalypso.model.wspm.sobek.core.model.BranchMaker;
import org.kalypso.model.wspm.sobek.core.model.Lastfall;
import org.kalypso.model.wspm.sobek.core.model.NodeUtils;
import org.kalypso.model.wspm.sobek.core.pub.FNNodeUtils;
import org.kalypso.model.wspm.sobek.core.utils.PiSobekModelUtils;
import org.kalypso.ogc.gml.FeatureUtils;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.repository.container.IRepositoryContainer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.xml.sax.SAXException;

/**
 * @author kuch
 */
public final class SobekModelMember implements ISobekModelMember
{
  private Feature m_modelMember;

  private static ISobekModelMember m_model = null;

  private final IRepositoryContainer m_reposContainer;

  private final IWorkspaceInterface m_workspace;

  private SobekModelMember( final IWorkspaceInterface workspace, final Feature modelMember, final IRepositoryContainer reposContainer )
  {
    m_workspace = workspace;
    m_reposContainer = reposContainer;
    if( modelMember == null )
      throw new IllegalStateException( "modelMember is null" );

    if( !ISobekConstants.QN_SOBEK_MODEL.equals( modelMember.getFeatureType().getQName() ) )
      throw new IllegalStateException( "modelMember is not of type: " + ISobekConstants.QN_SOBEK_MODEL_MEMBER );

    if( SobekModelMember.m_model == null )
    {
      m_modelMember = modelMember;
      SobekModelMember.m_model = this;
    }
  }

  public static ISobekModelMember getModel( )
  {
    return SobekModelMember.getModel( null, null, null );
  }

  /**
   * @param workspace
   *            CommandableWorspace instance for posting new feature, updating features, aso
   * @param modelMember
   *            Sobek model member
   * @param reposContainer
   *            Time Series repository container
   */
  public static ISobekModelMember getModel( final IWorkspaceInterface workspace, final Feature modelMember, final IRepositoryContainer reposContainer )
  {
    if( SobekModelMember.m_model == null && modelMember != null )
    {
      SobekModelMember.m_model = new SobekModelMember( workspace, modelMember, reposContainer );
      return SobekModelMember.m_model;
    }

    return SobekModelMember.m_model;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IModelMember#getBranchMembers()
   */
  public IBranch[] getBranchMembers( )
  {
    final List<IBranch> myBranches = new ArrayList<IBranch>();

    final List< ? > branches = (List< ? >) m_modelMember.getProperty( ISobekConstants.QN_HYDRAULIC_BRANCH_MEMBER );
    for( final Object object : branches )
    {
      if( !(object instanceof Feature) )
        continue;

      final Feature branch = (Feature) object;

      final IBranch myBranch = new Branch( this, branch );
      myBranches.add( myBranch );
    }

    return myBranches.toArray( new IBranch[] {} );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IModelMember#getCalculationLinkMembers()
   */
  public ICalculationLink[] getCalculationLinkMembers( )
  {
    throw new NotImplementedException();
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IModelMember#getLastfallMembers()
   */
  public ILastfall[] getLastfallMembers( )
  {
    final List<ILastfall> myLastfalls = new ArrayList<ILastfall>();

    final List< ? > lastfalls = (List< ? >) m_modelMember.getProperty( ISobekConstants.QN_HYDRAULIC_LASTFALL_MEMBER );
    for( final Object object : lastfalls )
    {
      if( !(object instanceof Feature) )
        continue;

      final Feature lastfall = (Feature) object;
      myLastfalls.add( new Lastfall( this, lastfall ) );
    }

    return myLastfalls.toArray( new ILastfall[] {} );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IModelMember#getNodeMembers()
   */
  public INode[] getNodeMembers( )
  {
    final List<INode> myNodes = new ArrayList<INode>();

    final List< ? > nodes = (List< ? >) m_modelMember.getProperty( ISobekConstants.QN_HYDRAULIC_NODE_MEMBER );
    for( final Object object : nodes )
    {
      if( !(object instanceof Feature) )
        continue;

      final Feature node = (Feature) object;
      myNodes.add( FNNodeUtils.getNode( this, node ) );
    }

    return myNodes.toArray( new INode[] {} );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IModelMember#getBranchMaker()
   */
  public IBranchMaker getBranchMaker( )
  {
    return new BranchMaker( this );
  }

  public Feature getFeature( )
  {
    return m_modelMember;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IModelMember#deleteFoo(org.kalypsodeegree.model.feature.Feature)
   */
  public void deleteFoo( final Feature feature ) throws Exception
  {
    final QName qn = feature.getFeatureType().getQName();

    if( ISobekConstants.QN_HYDRAULIC_SOBEK_BRANCH.equals( qn ) )
      new Branch( this, feature ).delete();
    else if( ISobekConstants.QN_HYDRAULIC_CROSS_SECTION_NODE.equals( qn ) )
      FeatureUtils.deleteFeature( m_workspace.getCommandableWorkspace(), feature );
    else
      throw new NotImplementedException();
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISobekModelMember#writePi(java.net.URL,
   *      org.kalypso.model.wspm.sobek.core.interfaces.ISobekModelMember.TARGET)
   * @author thuel2
   */
  public void writePi( final URL targetDir, final TARGET target ) throws IOException, GM_Exception
  {
    // ensure existence of targetDir
    final File fleTargetDir = new File( targetDir.getFile() );
    if( !fleTargetDir.isDirectory() )
      throw new IOException( "'" + targetDir + "' is not a directory" );
    if( !fleTargetDir.exists() )
      FileUtils.forceMkdir( fleTargetDir );

    final ObjectFactory factory = new ObjectFactory();
    JAXBElement< ? > xmlElements;
    String sFleXml;

    if( TARGET.eLocations.equals( target ) )
    {
      // root element Locations
      final LocationsComplexType locationsComplexType = factory.createLocationsComplexType();
      locationsComplexType.setGeoDatum( GeoDatumEnumStringType.LOCAL.value() );
      xmlElements = factory.createLocations( locationsComplexType );
      // list of nodes to be converted to locations
      final INode[] nodes = getNodeMembers();
      for( final INode node : nodes )
      {
        final Location location = PiSobekModelUtils.getInstance().createLocationFromNode( factory, node );
        locationsComplexType.getLocation().add( location );
      }
      // name of target file
      sFleXml = "nodes.xml";
    }
    else if( TARGET.eBranches.equals( target ) )
    {
      // root element Branches
      final BranchesComplexType branchesComplexType = factory.createBranchesComplexType();
      branchesComplexType.setGeoDatum( GeoDatumEnumStringType.LOCAL.value() );
      xmlElements = factory.createBranches( branchesComplexType );

      final IBranch[] branches = getBranchMembers();
      for( final IBranch branch : branches )
      {
        // FIXME
// final BranchComplexType piBranch = PiSobekModelUtils.getInstance().createPiBranchFromBranch( factory, branch );
// branchesComplexType.getBranch().add( piBranch );
      }
      // name of target file
      sFleXml = "Branches.xml";
    }
    else if( TARGET.eCrossSections.equals( target ) )
    {
      // root element crossSections
      final CrossSectionsComplexType crossSectionsComplexType = factory.createCrossSectionsComplexType();
      crossSectionsComplexType.setGeoDatum( GeoDatumEnumStringType.LOCAL.value() );
      xmlElements = factory.createCrossSections( crossSectionsComplexType );

      final ICrossSectionNode[] crossSectionNodes = getCrossSectionNodeMembers();
      for( final ICrossSectionNode csNode : crossSectionNodes )
      {
        // FIXME
// final CrossSection cs = PiSobekModelUtils.getInstance().createCrossSectionFromCSNode( factory, csNode );
// crossSectionsComplexType.getCrossSection().add( cs );
      }
      sFleXml = "CrossSections.xml";
      getNodeMembers();

    }
    else
      throw new NotImplementedException();

    // marshall
    final File fleXml = new File( targetDir.getFile(), sFleXml );
    final FileOutputStream os = new FileOutputStream( fleXml );
    final JAXBContext jc;
    try
    {
      jc = JAXBContext.newInstance( ObjectFactory.class );

      final Marshaller m = jc.createMarshaller();
      m.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );

      final SchemaFactory SCHEMA_FACTORY = SchemaFactory.newInstance( XMLConstants.W3C_XML_SCHEMA_NS_URI );
      final URL schemaURL = PluginUtilities.findResource( "org.kalypso.model.wspm.sobek.core", "etc/schemas/pi/fileformats.xsd" );
      final Schema schema = SCHEMA_FACTORY.newSchema( schemaURL );
      m.setSchema( schema );

      m.marshal( xmlElements, os );

      os.flush();
      os.close();

    }
    catch( final JAXBException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch( final SAXException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    finally
    {
      org.apache.commons.io.IOUtils.closeQuietly( os );
    }
  }

  /**
   *
   */
  private ICrossSectionNode[] getCrossSectionNodeMembers( )
  {
    final INode[] allNodes = getNodeMembers();
    final List<ICrossSectionNode> crossSectionsNodes = new ArrayList<ICrossSectionNode>();
    for( final INode node : allNodes )
      if( node instanceof ICrossSectionNode )
        crossSectionsNodes.add( (ICrossSectionNode) node );
    return crossSectionsNodes.toArray( new ICrossSectionNode[] {} );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IModelMember#getNodeUtils()
   */
  public INodeUtils getNodeUtils( )
  {
    return new NodeUtils( this );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISobekModelMember#getRepositoryContainer()
   */
  public IRepositoryContainer getRepositoryContainer( )
  {
    return m_reposContainer;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISobekModelMember#writePi(java.net.URL)
   */
  public void writePi( final URL targetDir ) throws IOException, GM_Exception
  {
    final TARGET[] values = TARGET.values();
    for( final TARGET target : values )
      writePi( targetDir, target );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IModelMember#getWorkspace()
   */
  public CommandableWorkspace getWorkspace( )
  {
    return m_workspace.getCommandableWorkspace();
  }
}
