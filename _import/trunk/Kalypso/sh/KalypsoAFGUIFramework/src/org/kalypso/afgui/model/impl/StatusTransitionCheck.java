/**
 * 
 */
package org.kalypso.afgui.model.impl;

import org.kalypso.afgui.model.EActivityExeState;
import org.kalypso.afgui.model.IStatusTransitionCheck;

class StatusTransitionCheck implements IStatusTransitionCheck
{
	/* (non-Javadoc)
	 * @see org.kalypso.afgui.model.IStatusTransitionCheck#checkTransiton(org.kalypso.afgui.model.EActivityStatus, org.kalypso.afgui.model.EActivityStatus)
	 */
	public boolean checkTransiton(
			EActivityExeState currentStatus,
			EActivityExeState nextStatus)
	{
		return (currentStatus==EActivityExeState.NOT_STARTED&&
					nextStatus==EActivityExeState.ON_GOING) ||
				(currentStatus==EActivityExeState.ON_GOING&&
						nextStatus==EActivityExeState.DONE)||
				(currentStatus==EActivityExeState.DONE&&
						nextStatus==EActivityExeState.ON_GOING);
	}
}