package org.kalypso.afgui.model;

public interface IStatusTransitionCheck
{

	public boolean checkTransiton(EActivityExeState currentStatus,
			EActivityExeState nextStatus);

}