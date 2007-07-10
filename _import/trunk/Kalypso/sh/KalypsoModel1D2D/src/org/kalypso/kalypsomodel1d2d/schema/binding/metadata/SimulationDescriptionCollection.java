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
package org.kalypso.kalypsomodel1d2d.schema.binding.metadata;

import java.io.File;

import javax.xml.namespace.QName;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IResultModel1d2d;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.FeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;

import de.renew.workflow.connector.cases.CaseHandlingSourceProvider;

/**
 * @author Patrice Congo
 * @author Dejan Antanaskovic
 * 
 */
public class SimulationDescriptionCollection extends AbstractFeatureBinder implements ISimulationDescriptionCollection
{
  private IFeatureWrapperCollection<IModelDescriptor> m_modelDescriptors;
  private IFeatureWrapperCollection<ISimulationDescriptor> m_simulationDescriptors;

  public SimulationDescriptionCollection( Feature featureToBind)
  {
    this(
        featureToBind, 
        Kalypso1D2DSchemaConstants.SIMMETA_F_SIMDESCRIPTOR_COLLECTION );
  }
  
  public SimulationDescriptionCollection( Feature featureToBind, QName qnameToBind )
  {
    super( featureToBind, qnameToBind );
    m_modelDescriptors = new FeatureWrapperCollection<IModelDescriptor>(featureToBind, IModelDescriptor.class, Kalypso1D2DSchemaConstants.SIMMETA_PROP_MODELDESCRIPTOR);
    m_simulationDescriptors = 
      new FeatureWrapperCollection<ISimulationDescriptor>(featureToBind, ISimulationDescriptor.class, Kalypso1D2DSchemaConstants.SIMMETA_PROP_SIMDESCRIPTOR);
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.metadata.ISimulationDescriptionCollection#getModelDescriptors()
   */
  public IFeatureWrapperCollection<IModelDescriptor> getModelDescriptors( )
  {
    return m_modelDescriptors;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.metadata.ISimulationDescriptionCollection#getSimulationDescriptors()
   */
  public IFeatureWrapperCollection<ISimulationDescriptor> getSimulationDescriptors( )
  {
    return m_simulationDescriptors;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.metadata.ISimulationDescriptionCollection#addModelDescriptor(org.kalypsodeegree.model.feature.binding.IFeatureWrapper2)
   */
  public IModelDescriptor addModelDescriptor( IFeatureWrapper2 modelFeatureWrapper )
  {
    IModelDescriptor existingEntry = getExistingEntry( modelFeatureWrapper );
    if( existingEntry != null )
    {
      return existingEntry;
    }
    Assert.throwIAEOnNullParam( modelFeatureWrapper, "modelFeatureWrapper" );
    final QName newChildType;
    if( modelFeatureWrapper instanceof IResultModel1d2d )
    {
      newChildType = Kalypso1D2DSchemaConstants.SIMMETA_F_RESULT;
    }
    else
    {
      newChildType = Kalypso1D2DSchemaConstants.SIMMETA_F_MODELDESCRIPTOR;
    }
    IModelDescriptor addNew = m_modelDescriptors.addNew( newChildType );
    String modelGmlID = modelFeatureWrapper.getGmlID();
    addNew.setModelID( modelGmlID );
    
    String name = modelFeatureWrapper.getName();
    addNew.setModelName( isNullOrEmptyString( name )?modelGmlID:name );
    addNew.setModelType( modelFeatureWrapper.getWrappedFeature().getFeatureType().getQName().toString() );
    
    //workspace path
//    final IWorkbench workbench = PlatformUI.getWorkbench();
//    final IHandlerService service = (IHandlerService) workbench.getService( IHandlerService.class );
//    final IEvaluationContext currentState = service.getCurrentState();
//    final IFolder scenarioFolder = (IFolder) currentState.getVariable( CaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );
//    IFile file = scenarioFolder.getFile( modelFeatureWrapper.getWrappedFeature().getWorkspace().getContext().getFile() );
    
    final GMLWorkspace workspace = modelFeatureWrapper.getWrappedFeature().getWorkspace();
    addNew.setWorkspacePath( new File(workspace.getContext().getFile()).toString() );
    
//    addNew.setWorkspacePath( file.getFullPath().toOSString() );
    return addNew;
  }
  
  private boolean isNullOrEmptyString(String str)
  {
   if( str==null)
   {
     return true;
   }
   else if( str.trim().length()==0)
   {
     return true;
   }
   else
   {
     return false;
   }
  }
  
  public IModelDescriptor getExistingEntry(IFeatureWrapper2 featureWrapper2 )
  {
    GMLWorkspace workspace = featureWrapper2.getWrappedFeature().getWorkspace();
    final String path = workspace.getContext().toString();
    final String featureGmlID = featureWrapper2.getGmlID();
    for(IModelDescriptor desc:m_modelDescriptors)
    {
     //String descPath = desc.getWorkspacePath();
     //path cannot be used since calculation uses tm file
    if( /*path.equals( descPath ) &&*/
         featureGmlID.equals( desc.getModelID() ) )
     {
       return desc;
     }
    }
    
    return null;
  }
  
  
  
}
