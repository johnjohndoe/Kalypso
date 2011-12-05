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
package org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.kalypso.contribs.java.lang.MultiException;
import org.kalypso.kalypsomodel1d2d.ops.CalcUnitOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

/**
 * Command to add element to calculation unit
 * 
 * @author Patrice Congo
 * 
 */
@SuppressWarnings("unchecked")//$NON-NLS-1$
public class AddElementToCalculationUnitCmd implements IDiscrModel1d2dChangeCommand
{

  private final IFE1D2DElement[] m_elementsToAdd;

  private boolean added = false;

  private final ICalculationUnit m_calculationUnit;

  private final IFEDiscretisationModel1d2d m_model1d2d;

  public AddElementToCalculationUnitCmd( ICalculationUnit calculationUnit, IFE1D2DElement[] elementsToAdd, IFEDiscretisationModel1d2d model1d2d )
  {
    m_calculationUnit = calculationUnit;

    m_elementsToAdd = CalcUnitOps.toAddableElements( calculationUnit, elementsToAdd );
    m_model1d2d = model1d2d;
  }

  public AddElementToCalculationUnitCmd( ICalculationUnit calculationUnit, Feature[] elementsToAdd, IFEDiscretisationModel1d2d model1d2d )
  {
    m_calculationUnit = calculationUnit;

    m_elementsToAdd = CalcUnitOps.toAddableElements( calculationUnit, elementsToAdd );
    m_model1d2d = model1d2d;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand#getChangedFeature()
   */
  @Override
  public IFeatureWrapper2[] getChangedFeature( )
  {
    if( added )
    {
      List<IFeatureWrapper2> changed = new ArrayList<IFeatureWrapper2>();
      changed.addAll( Arrays.asList( m_elementsToAdd ) );
      return changed.toArray( new IFeatureWrapper2[changed.size()] );
    }
    else
    {
      return new IFeatureWrapper2[] {};
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand#getDiscretisationModel1d2d()
   */
  @Override
  public IFEDiscretisationModel1d2d getDiscretisationModel1d2d( )
  {
    return m_model1d2d;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  @Override
  public String getDescription( )
  {
    return "Elemente einer Berechnungseinheit hinzufügen"; //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  @Override
  public boolean isUndoable( )
  {
    return true;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  @Override
  public void process( ) throws Exception
  {
    if( !added )
    {
      try
      {
        for( IFE1D2DElement ele : m_elementsToAdd )
        {
          ele.getContainers().addRef( m_calculationUnit );
          m_calculationUnit.addElementAsRef( ele );
        }

        added = true;
        // fire change
        fireProcessChanges();
      }
      catch( Exception th )
      {
        for( IFE1D2DElement ele : m_elementsToAdd )
        {
          try
          {
            ele.getContainers().add( m_calculationUnit );
          }
          catch( Throwable e )
          {

          }
          try
          {
            m_calculationUnit.addElementAsRef( ele );
          }
          catch( Throwable e )
          {

          }
        }
        th.printStackTrace();
        throw th;
      }

    }
  }

  private final void fireProcessChanges( )
  {
    List<Feature> features = new ArrayList<Feature>( m_elementsToAdd.length * 2 );
    features.add( m_calculationUnit.getFeature() );
    for( IFE1D2DElement ele : m_elementsToAdd )
      features.add( ele.getFeature() );

    GMLWorkspace workspace = m_calculationUnit.getFeature().getWorkspace();
    FeatureStructureChangeModellEvent event = new FeatureStructureChangeModellEvent( workspace,// final GMLWorkspace

    // workspace,
    m_model1d2d.getFeature(),// Feature parentFeature,
    features.toArray( new Feature[features.size()] ),// final Feature[] changedFeature,
    FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD// final int changeType
    );
    workspace.fireModellEvent( event );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  @Override
  public void redo( ) throws Exception
  {
    if( !added )
      process();
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  @Override
  public void undo( ) throws Exception
  {
    if( added )
    {
      MultiException multiException = null;
      for( IFE1D2DElement ele : m_elementsToAdd )
      {
        try
        {
          ele.getContainers().add( m_calculationUnit );
        }
        catch( Exception e )
        {
          e.printStackTrace();
          if( multiException == null )
          {
            multiException = new MultiException();
          }
          multiException.addException( e );
        }
        try
        {
          m_calculationUnit.addElementAsRef( ele );
        }
        catch( Exception e )
        {
          if( multiException == null )
          {
            multiException = new MultiException();
          }
          multiException.addException( e );
        }
      }
      if( multiException != null )
      {
        throw multiException;
      }
      else
      {
        added = false;
        fireProcessChanges();
      }
    }
  }

}
