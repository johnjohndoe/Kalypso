package org.kalypso.afgui.db;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

import org.kalypso.afgui.model.ITaskGroup;
import org.kalypso.afgui.model.IWorkflowConcept;
import org.kalypso.afgui.model.internal.TaskGroup;

import com.hp.hpl.jena.rdf.model.NodeIterator;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Seq;

class TaskGroupSeq implements IWorkflowConcept, List<ITaskGroup> {
	final private Seq seq;

	public TaskGroupSeq(Seq seq) {
		if (seq == null) {
			throw new IllegalArgumentException();
		}
		this.seq = seq;
	}

	public TaskGroupSeq(Resource resource) {
		if (resource instanceof Seq) {
			this.seq = (Seq) resource;
		} else {
			throw new IllegalArgumentException();
		}
	}

	public boolean add(ITaskGroup o) {
		if (o == null) {
			return false;
		} else {
			Resource res = (Resource) o.getModelObject();
			if (res == null) {
				return false;
			} else {
				seq.add(res);
				return true;
			}
		}
	}

	public void add(int index, ITaskGroup element) {
		assertIndexInBound(index, size());

		if (element == null) {
			throw new IllegalArgumentException();
		} else {
			Resource resource = (Resource) element.getModelObject();
			if (resource == null) {
				throw new IllegalArgumentException();
			} else {
				seq.add(toSeqIndex(index), element);
			}
		}

	}

	public boolean addAll(Collection<? extends ITaskGroup> c) {
		throw new UnsupportedOperationException();
	}

	public boolean addAll(int index, Collection<? extends ITaskGroup> c) {
		throw new UnsupportedOperationException();
	}

	public void clear() {
		for (int i = size(); i > 1; i--) {
			seq.remove(i);
		}
	}

	public boolean contains(Object o) {
		if (o == null) {
			return false;
		} else if (o instanceof ITaskGroup) {
			return seq.contains(((ITaskGroup) o).getModelObject());
		} else {
			return false;
		}
	}

	public boolean containsAll(Collection<?> c) {
		if (c == null) {
			return false;
		} else {
			for (Object o : c) {
				if (!contains(o)) {
					return false;
				}
			}
			return true;
		}
	}

	public ITaskGroup get(int index) {
		assertIndexInBound(index, size());
		try {
			return new TaskGroup(seq.getResource(toSeqIndex(index)));
		} catch (Throwable th) {
			throw new RuntimeException("bad type in sequence");
		}
	}

	public int indexOf(Object o) {
		if (o instanceof ITaskGroup) {
			Resource res = (Resource) ((ITaskGroup) o).getModelObject();
			if (res == null) {
				return -1;
			} else {
				int seqIndex = seq.indexOf(res);
				return seqIndex - 1;
			}
		} else {
			return -1;
		}
	}

	public boolean isEmpty() {
		return seq.size() == 0;
	}

	public Iterator<ITaskGroup> iterator() {
		return new Iterator<ITaskGroup>() {
			NodeIterator it = seq.iterator();

			public boolean hasNext() {
				return it.hasNext();
			}

			public ITaskGroup next() {
				return new TaskGroup((Resource) it.nextNode());
			}

			public void remove() {
				throw new UnsupportedOperationException();
			}

		};
	}

	public int lastIndexOf(Object o) {
		throw new UnsupportedOperationException();
	}

	public ListIterator<ITaskGroup> listIterator() {
		throw new UnsupportedOperationException();
	}

	public ListIterator<ITaskGroup> listIterator(int index) {
		throw new UnsupportedOperationException();
	}

	public boolean remove(Object o) {
		if (o instanceof ITaskGroup) {
			Resource res = (Resource) ((ITaskGroup) o).getModelObject();
			if (res == null) {
				throw new IllegalArgumentException();
			} else {
				int i = seq.indexOf(res);
				if (i != 0) {
					return false;
				} else {
					seq.remove(i);
					return true;
				}
			}
		} else {
			return false;
		}
	}

	public ITaskGroup remove(int index) {
		assertIndexInBound(index, size());
		Resource res = seq.getResource(toSeqIndex(index));
		return new TaskGroup(res);
	}

	public boolean removeAll(Collection<?> c) {
		throw new UnsupportedOperationException();
	}

	public boolean retainAll(Collection<?> c) {
		throw new UnsupportedOperationException();
	}

	public ITaskGroup set(int index, ITaskGroup element) {
		assertIndexInBound(index, size());
		ITaskGroup oldTask = new TaskGroup(seq.getResource(index));
		seq.set(index + 1, element.getModelObject());
		return oldTask;
	}

	public int size() {
		return seq.size();
	}

	public List<ITaskGroup> subList(int fromIndex, int toIndex) {
		throw new UnsupportedOperationException();
	}

	public Object[] toArray() {
		return null;
	}

	public <T> T[] toArray(T[] a) {
		return null;
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof TaskGroupSeq) {
			try {
				return seq.equals(((TaskGroupSeq) obj).seq);
			} catch (Throwable th) {
				return false;
			}
		} else {
			return false;
		}
	}

	private static final void assertIndexInBound(int index, int size) {
		if (index >= size || index < 0) {
			throw new IndexOutOfBoundsException();
		}
	}

	private static final int toSeqIndex(int index) {
		return index + 1;
	}
}
