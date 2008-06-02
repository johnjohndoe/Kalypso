package org.kalypso.model.wspm.sobek.result.processing.model;

import java.util.List;

import org.kalypso.model.wspm.sobek.core.interfaces.IBranch;

public interface IBranchHydrographModel extends List
{

  IBranchHydrograph getHydrograph( IBranch branch );

}
