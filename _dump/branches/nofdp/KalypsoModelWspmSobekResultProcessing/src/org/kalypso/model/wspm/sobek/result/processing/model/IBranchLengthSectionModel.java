package org.kalypso.model.wspm.sobek.result.processing.model;

import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.model.wspm.sobek.core.interfaces.IBranch;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.model.wspm.sobek.result.processing.i18n.Messages;

public interface IBranchLengthSectionModel extends List
{
// {org.kalypso.model.wspm.sobek.result.ls}ResultLsList
// - {org.kalypso.model.wspm.sobek.result.ls}resultLsMember

  public static final QName QN_HYDROGRAPHS = new QName( ISobekConstants.NS_SOBEK_RESULT_BRANCH_HYDROGRAPHS, "resultLsMember" ); //$NON-NLS-1$

  IBranchLengthSection getHydrograph( IBranch branch ) throws CoreException;

  IBranchLengthSection[] getLengthSections( );
}
