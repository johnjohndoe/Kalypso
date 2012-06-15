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
package org.kalypso.kalypsomodel1d2d.ui.map.cmds.ele;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.commons.command.ICommand;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.IFeatureChangeCommand;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModelSystem;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.NativeTerrainElevationModelFactory;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

/**
 * Composite command used to change the discretisation command. This composite takes the responsibility to nodifies the
 * commandable workspace about the chnage introduced by its sub command
 * 
 * 
 * @author Patrice Congo
 * 
 */
public class ChangeTerrainElevationSystemCommand implements ICommand
{
  public static final String DEFAULT_DESCRIPTION = Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.cmds.ele.ChangeTerrainElevationSystemCommand.0"); //$NON-NLS-1$

  private final CommandableWorkspace m_commandableWorkspace;

  private final List<IFeatureChangeCommand> m_commands = new ArrayList<IFeatureChangeCommand>();

  private final List<IFile> m_files = new ArrayList<IFile>();

  private boolean isUndoable = true;

  private boolean m_toDelete = false;
  
  private final IFEDiscretisationModel1d2d m_model1d2d;

  private final String m_description;

  private ITerrainElevationModelSystem m_terrainModel;

  public ChangeTerrainElevationSystemCommand( final CommandableWorkspace commandableWorkspace, final IFEDiscretisationModel1d2d model1d2d, final ITerrainElevationModelSystem terrainModel )
  {
    this( commandableWorkspace, model1d2d, terrainModel, DEFAULT_DESCRIPTION );
  }
  
  public ChangeTerrainElevationSystemCommand( final CommandableWorkspace commandableWorkspace, final IFEDiscretisationModel1d2d model1d2d )
  {
    this( commandableWorkspace, model1d2d, null, DEFAULT_DESCRIPTION );
  }

  public ChangeTerrainElevationSystemCommand( final CommandableWorkspace commandableWorkspace, final IFEDiscretisationModel1d2d model1d2d, final ITerrainElevationModelSystem terrainModel, final String description )
  {
    Assert.throwIAEOnNullParam( model1d2d, Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.cmds.ele.ChangeTerrainElevationSystemCommand.1") ); //$NON-NLS-1$
    Assert.throwIAEOnNullParam( description, Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.cmds.ele.ChangeTerrainElevationSystemCommand.2") ); //$NON-NLS-1$
    m_commandableWorkspace = commandableWorkspace;
    m_model1d2d = model1d2d;
    m_description = description;
    m_terrainModel = terrainModel;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  @Override
  public String getDescription( )
  {
    return m_description;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  @Override
  public boolean isUndoable( )
  {
    return isUndoable;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  @Override
  @SuppressWarnings("deprecation")
  public void process( ) throws Exception
  {
    final List<Feature> changedFeatures = new ArrayList<Feature>();
    for( final IFeatureChangeCommand command : m_commands )
    {
      try
      {
        command.process();
        if( command instanceof DeleteNativeTerrainElevationWrapper ){
          m_toDelete = true;
        }
        final IFeatureWrapper2[] changedFeatures2 = command.getChangedFeature();
        if( changedFeatures2 != null )
        {
          for( final IFeatureWrapper2 changedFeature : changedFeatures2 )
          {
            if( changedFeature != null )
            {
              final Feature wrappedFeature = changedFeature.getFeature();
              if( wrappedFeature != null )
              {
                changedFeatures.add( wrappedFeature );
                wrappedFeature.invalidEnvelope();
              }
            }
          }
        }
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
    }

    if( m_toDelete ){
      fireElementsRemove( changedFeatures );
    }
    else{
      fireStructureChange( changedFeatures );
      // why is this needed
      m_model1d2d.getEdges().getWrappedList().invalidate();
      m_model1d2d.getElements().getWrappedList().invalidate();
      m_model1d2d.getNodes().getWrappedList().invalidate();
    }
  }

  private void fireElementsRemove( List<Feature> changedFeatures )
  {
    final Feature[] changedFeaturesArray = new Feature[changedFeatures.size()];
    changedFeatures.toArray( changedFeaturesArray );
    m_commandableWorkspace.fireModellEvent( new FeatureStructureChangeModellEvent( m_commandableWorkspace, m_terrainModel.getFeature(), changedFeaturesArray, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE ) );
  }

  private final void fireStructureChange( final List<Feature> changedFeatures )
  {
    final Feature[] changedFeaturesArray = new Feature[changedFeatures.size()];
    changedFeatures.toArray( changedFeaturesArray );
    m_commandableWorkspace.fireModellEvent( new FeatureStructureChangeModellEvent( m_commandableWorkspace, m_model1d2d.getFeature(), changedFeaturesArray, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_MOVE ) );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  @Override
  public void redo( ) throws Exception
  {
    for( final IFeatureChangeCommand command : m_commands )
    {
      try
      {
        command.redo();
      }
      catch( final Throwable th )
      {
        th.printStackTrace();
      }
    }
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  @Override
  public void undo( ) throws Exception
  {

    // reverse order is taken because of eventual dependencies
    IFeatureChangeCommand command;
    for( int index = m_commands.size() - 1; index >= 0; index-- )
    {
      command = m_commands.get( index );
      try
      {
        command.undo();
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
    }

  }

  public void addCommand( final IFeatureChangeCommand command, final IFile file )
  {
    Assert.throwIAEOnNullParam( command, Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.cmds.ele.ChangeTerrainElevationSystemCommand.3") ); //$NON-NLS-1$
    m_commands.add( command );
    if( file != null )
      m_files.add( file );

    isUndoable = isUndoable && command.isUndoable();

  }

  public IStatus deleteFiles( )
  {
    final MultiStatus status = new MultiStatus( KalypsoModel1D2DPlugin.getDefault().getBundle().getSymbolicName(), 1, Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.cmds.ele.ChangeTerrainElevationSystemCommand.4"), null ); //$NON-NLS-1$
    for( final IFile file : m_files )
    {
      try
      {
        file.delete( false, new NullProgressMonitor() );

        /* remove from cache */
        final File chachedFile = file.getLocation().toFile();
        NativeTerrainElevationModelFactory.removeFromCache( chachedFile );
      }
      catch( final CoreException e )
      {
        status.add( e.getStatus() );
      }
    }

    return status;
  }

  public IStatus removeFromCahce( )
  {

    return null;

  }

}
