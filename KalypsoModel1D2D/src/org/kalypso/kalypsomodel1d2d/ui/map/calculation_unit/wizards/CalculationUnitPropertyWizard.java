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
package org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.wizards;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.ObjectUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.commons.command.ICommandManager;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IJunctionElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ITransitionElement;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.AddSubCalcUnitsToCalcUnit1D2DCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.RemoveSubCalcUnitsFromCalcUnit1D2DCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.ogc.gml.command.ChangeFeatureCommand;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

/**
 * @author Madanagopal
 * @author Dejan Antanaskovic
 * @author Gernot Belger
 */
public class CalculationUnitPropertyWizard extends Wizard
{
  private final CalculationUnitDataModel m_dataModel;

  private final CalculationUnitPropertyWizardPage m_wizardPage;

  private final ICalculationUnit m_parentCalcUnit;

  public CalculationUnitPropertyWizard( final CalculationUnitDataModel dataModel )
  {
    m_dataModel = dataModel;

    m_parentCalcUnit = m_dataModel.getSelectedCalculationUnit();
    final ICalculationUnit[] calculationUnits = m_dataModel.getCalculationUnits();

    m_wizardPage = new CalculationUnitPropertyWizardPage( m_parentCalcUnit, calculationUnits );

    setWindowTitle( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.wizards.CalculationUnitPropertyWizard.0") );  //$NON-NLS-1$
  }

  @Override
  public void addPages( )
  {
    addPage( m_wizardPage );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    final ICalculationUnit[] newSubUnits = m_wizardPage.getSelectedSubUnits();
    if( !(m_parentCalcUnit instanceof ICalculationUnit1D2D) || checkSubUnitsInterconnection( newSubUnits ) )
    {
      try
      {
        final ICommandManager commandManager = m_dataModel.getData( ICommandManager.class, ICommonKeys.KEY_COMMAND_MANAGER_DISC_MODEL );
        final Feature calcUnitFeature = m_parentCalcUnit.getFeature();

        final String newName = m_wizardPage.getChangedName();
        if( !ObjectUtils.equals( m_parentCalcUnit.getName(), newName ) )
        {
          final List<String> nameList = new ArrayList<String>();
          nameList.add( newName );
          final ChangeFeatureCommand changeNameCmd = new ChangeFeatureCommand( calcUnitFeature, calcUnitFeature.getFeatureType().getProperty( Feature.QN_NAME ), nameList );
          commandManager.postCommand( changeNameCmd );
        }

        if( m_parentCalcUnit instanceof ICalculationUnit1D )
        {
          final ICalculationUnit1D calcUnit1D = (ICalculationUnit1D) m_parentCalcUnit;
          final int newInterpolationCount = m_wizardPage.getChangedInterpolationCount();
          if( !ObjectUtils.equals( calcUnit1D.getInterpolationCount(), newInterpolationCount ) )
          {
            final ChangeFeatureCommand changeNameCmd = new ChangeFeatureCommand( calcUnitFeature, calcUnitFeature.getFeatureType().getProperty( ICalculationUnit1D.QNAME_PROP_INTERP_COUNT ), new BigInteger( "" //$NON-NLS-1$
                + newInterpolationCount ) );
            commandManager.postCommand( changeNameCmd );
          }
        }

        if( m_parentCalcUnit instanceof ICalculationUnit1D2D )
        {
          final ICalculationUnit1D2D calcUnit1D2D = (ICalculationUnit1D2D) m_parentCalcUnit;
          final IFeatureWrapperCollection<ICalculationUnit> subUnits = calcUnit1D2D.getChangedSubUnits();

          final RemoveSubCalcUnitsFromCalcUnit1D2DCmd cmdToRemove = new RemoveSubCalcUnitsFromCalcUnit1D2DCmd( subUnits.toArray( new ICalculationUnit[subUnits.size()] ), calcUnit1D2D );
          final AddSubCalcUnitsToCalcUnit1D2DCmd cmdToAdd = new AddSubCalcUnitsToCalcUnit1D2DCmd( newSubUnits, calcUnit1D2D );

          commandManager.postCommand( cmdToRemove );
          commandManager.postCommand( cmdToAdd );
        }
      }
      catch( final Exception e )
      {
        final IStatus status = StatusUtilities.statusFromThrowable( e );
        KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
        ErrorDialog.openError( getShell(), getWindowTitle(), Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.wizards.CalculationUnitPropertyWizard.2"), status ); //$NON-NLS-1$
      }

      return true;
    }
    else
    {
      MessageDialog.openWarning( getShell(), Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.wizards.CalculationUnitPropertyWizard.3"), Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.wizards.CalculationUnitPropertyWizard.4") );   //$NON-NLS-1$ //$NON-NLS-2$
      return false;
    }
  }

  private boolean checkSubUnitsInterconnection( final ICalculationUnit[] inputListCalSubUnits )
  {
    final List<ICalculationUnit> connectedUnits = new ArrayList<ICalculationUnit>();
    connectedUnits.add( inputListCalSubUnits[0] );

    for( final ICalculationUnit subUnitsToGoThrough : inputListCalSubUnits )
    {
      for( final ICalculationUnit currentUnit : inputListCalSubUnits )
      {
        if( connectedUnits.contains( currentUnit ) )
          continue;
        else
        {
          if( internalCheckInterconnection( connectedUnits, currentUnit ) )
          {
            connectedUnits.add( currentUnit );
          }
          else
            continue;
        }
      }
    }
    if( connectedUnits.size() == inputListCalSubUnits.length )
      return true;
    else
      return false;
  }

  private boolean internalCheckInterconnection( final List<ICalculationUnit> connectedUnits, final ICalculationUnit currentUnit )
  {
    if( connectedUnits.size() == 0 )
      return true;

    // -----------------------------------------------------------------------------
    // check if current unit have common continuity line with any unit from the list
    // -----------------------------------------------------------------------------
    final List<IFELine> currentUnitLines = currentUnit.getContinuityLines();
    for( final ICalculationUnit connectedUnit : connectedUnits )
    {
      // if calculation units are of different type, common continuity line cannot exists
      if( !connectedUnit.getType().equals( currentUnit.getType() ) )
        continue;

      final List<IFELine> connectedUnitLines = connectedUnit.getContinuityLines();
      for( final IFELine line : connectedUnitLines )
        if( currentUnitLines.contains( line ) )
          return true;
    }

    // --------------------------------------------------------------------------------
    // check if current unit have common transition element with any unit from the list
    // --------------------------------------------------------------------------------
    final List<IFELine> currentUnitContinuityLines = currentUnit.getContinuityLines();
    final IFEDiscretisationModel1d2d model = (IFEDiscretisationModel1d2d) m_dataModel.getData( ICommonKeys.KEY_DISCRETISATION_MODEL );
    final IFeatureWrapperCollection<IFE1D2DComplexElement> complexElements = model.getComplexElements();

    // get all transition elements from the discretisation model
    final List<ITransitionElement> allTransitionElements = new ArrayList<ITransitionElement>();
    for( final IFE1D2DComplexElement complexElement : complexElements )
      if( complexElement instanceof ITransitionElement )
        allTransitionElements.add( (ITransitionElement) complexElement );

    // get all transition elements between already connected units
    final List<ITransitionElement> connectedTransitionElements = new ArrayList<ITransitionElement>();
    for( final ITransitionElement transitionElement : allTransitionElements )
    {
      for( final ICalculationUnit connectedUnit : connectedUnits )
      {
        final List<IFELine> connectedUnitLines = connectedUnit.getContinuityLines();
        for( final IFELine line : connectedUnitLines )
          if( transitionElement.getContinuityLines().contains( line ) )
            if( !connectedTransitionElements.contains( transitionElement ) )
              connectedTransitionElements.add( transitionElement );
      }
    }

    // check if current unit is connected to any of the connected transition elements
    for( final IFELine line : currentUnitContinuityLines )
    {
      for( final ITransitionElement transitionElement : connectedTransitionElements )
      {
        if( transitionElement.getContinuityLines().contains( line ) )
          return true;
      }
    }

    // ------------------------------------------------------------------------------
    // check if current unit have common junction element with any unit from the list
    // ------------------------------------------------------------------------------
    if( currentUnit instanceof ICalculationUnit1D )
    {
      // get all junction elements from the discretisation model
      final List<IJunctionElement> allJunctionElements = new ArrayList<IJunctionElement>();
      for( final IFE1D2DComplexElement complexElement : complexElements )
        if( complexElement instanceof IJunctionElement )
          allJunctionElements.add( (IJunctionElement) complexElement );

      // get all junction elements between already connected units
      final List<IJunctionElement> connectedJunctionElements = new ArrayList<IJunctionElement>();
      for( final IJunctionElement junctionElement : allJunctionElements )
      {
        for( final ICalculationUnit connectedUnit : connectedUnits )
        {
          if( connectedUnit instanceof ICalculationUnit1D )
          {
            final List<IFELine> connectedUnitLines = connectedUnit.getContinuityLines();
            for( final IFELine line : connectedUnitLines )
              if( junctionElement.getContinuityLines().contains( line ) )
                if( !connectedJunctionElements.contains( junctionElement ) )
                  connectedJunctionElements.add( junctionElement );
          }
        }
      }

      // check if current unit is connected to any of the connected transition elements
      for( final IFELine line : currentUnitContinuityLines )
      {
        for( final IJunctionElement junctionElement : connectedJunctionElements )
        {
          if( junctionElement.getContinuityLines().contains( line ) )
            return true;
        }
      }
    }
    // ------------------------------------------------------------------------------

    return false;
  }
}
