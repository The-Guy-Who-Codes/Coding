from qiskit import QuantumCircuit, execute, IBMQ
from qiskit.visualization import plot_histogram

qc = QuantumCircuit(4, 2)

qc.x(0)
qc.x(1)

qc.cx(0, 2)
qc.cx(1, 2)

qc.ccx(0, 1, 3)

qc.measure([3, 2], [0, 1])

provider = IBMQ.load_account()

backend = provider.get_backend('ibmq_belem')

job = execute(qc, backend)

job.wait_for_final_state()

result = job.result()
counts = result.get_counts()
plot_histogram(counts)